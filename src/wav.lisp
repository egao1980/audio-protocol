;;;; AU0 WAV: RIFF/WAVE files to and from PCM-BUFFERs.
;;;;
;;;; Encodings (+WAV-ENCODINGS+), each with its fmt chunk:
;;;;   :PCM16    format tag 1 (PCM), 16 bits: signed little-endian; code c is c / 32768
;;;;   :PCM24    format tag 1 (PCM), 24 bits: signed little-endian, 3 bytes; c / 8388608
;;;;   :FLOAT32  format tag 3 (IEEE float), 32 bits: little-endian IEEE 754 single
;;;; Every integer field of a WAV file is little-endian. Byte I/O builds on
;;;; binary-protocol 0.1.1 (PACK and UNPACK for the fixed-width fields, 16-bit
;;;; codes and float32; 0.1.1 reads IEEE infinities and NaNs without trapping).
;;;; 24-bit codes are a gap there (egao1980/binary-protocol#3), and
;;;; UNPACK-FROM's per-call buffer scan (#4) means unpacking fresh slices, as
;;;; midi-protocol does.
;;;;
;;;; (READ-WAV source) -> (values buffer encoding)
;;;;   SOURCE is an octet vector ((VECTOR (UNSIGNED-BYTE 8)); offsets count from
;;;;   index 0), a binary input stream of element type (UNSIGNED-BYTE 8)
;;;;   (offsets count from its position at the call; READ-WAV reads no byte
;;;;   after the data chunk's last sample byte, so the stream is left there), or
;;;;   a pathname designator, opened with that element type. Anything else is a
;;;;   TYPE-ERROR. ENCODING is the keyword of +WAV-ENCODINGS+.
;;;;
;;;; Layout read. The source starts "RIFF" <size u32> "WAVE". Then chunks, each
;;;; <id: 4 bytes> <size u32> <size bytes> and one pad byte when SIZE is odd
;;;; (not counted in SIZE), are read in order until the data chunk:
;;;;   "fmt "  the first one is the format (below); a later one is skipped.
;;;;   "data"  the samples. Reading stops after it: nothing after the data
;;;;           chunk (LIST, cue, a second data chunk, trailing bytes) is read.
;;;;   other   skipped by SIZE and its pad byte: LIST (INFO metadata), fact,
;;;;           PEAK, JUNK, bext, cue, smpl, ... are not interpreted.
;;;; The RIFF size field is not checked (writers that stream leave it wrong).
;;;;
;;;; The fmt chunk, at its start (all u16 but RATE and BYTES-PER-SECOND, u32):
;;;;   <tag> <channels> <rate> <bytes-per-second> <block-align> <bits>
;;;;   and, when SIZE >= 18, <cb-size>; for tag #xFFFE (WAVE_FORMAT_EXTENSIBLE)
;;;;   then <valid-bits u16> <channel-mask u32> <sub-format GUID, 16 bytes>.
;;;; Checks, in this order; each failure is at the fmt chunk's offset (its "f"):
;;;;   1. SIZE < 16: AUDIO-FORMAT-ERROR.
;;;;   2. TAG not 1, 3 or #xFFFE: WAV-UNSUPPORTED-ENCODING (FORMAT-TAG TAG,
;;;;      BITS BITS, as for every WAV-UNSUPPORTED-ENCODING below).
;;;;   3. TAG #xFFFE: SIZE < 40 or CB-SIZE < 22 is an AUDIO-FORMAT-ERROR. The
;;;;      GUID is standard when its last 14 bytes are 00 00 00 00 10 00 80 00 00
;;;;      AA 00 38 9B 71; its first two (u16) are then the sub-format tag, which
;;;;      replaces TAG below. A non-standard GUID is WAV-UNSUPPORTED-ENCODING
;;;;      with FORMAT-TAG #xFFFE. Then VALID-BITS other than 0 and BITS is
;;;;      WAV-UNSUPPORTED-ENCODING (FORMAT-TAG the sub-format tag). The channel
;;;;      mask is ignored.
;;;;   4. (TAG BITS) must be (1 16), (1 24) or (3 32), the three encodings;
;;;;      anything else (8-bit or 32-bit PCM, float64, A-law, mu-law, ADPCM ...)
;;;;      is WAV-UNSUPPORTED-ENCODING with FORMAT-TAG TAG and BITS.
;;;;   5. CHANNELS 0 or RATE 0: AUDIO-FORMAT-ERROR.
;;;;   6. BLOCK-ALIGN /= CHANNELS * BITS / 8: AUDIO-FORMAT-ERROR.
;;;; BYTES-PER-SECOND is not checked.
;;;;
;;;; The data chunk. FRAMES = floor(SIZE / BLOCK-ALIGN); the buffer has RATE,
;;;; CHANNELS, FRAMES and the samples in file order, each code decoded as the
;;;; encodings table says (exactly). Leftover bytes (SIZE not a multiple of
;;;; BLOCK-ALIGN) are not read, with a warning. A data chunk of size 0 gives a
;;;; buffer of 0 frames.
;;;;
;;;; Errors (AUDIO-FORMAT-ERROR unless named; OFFSET in brackets). A field or
;;;; chunk is checked once all of its bytes are there; until then, running out
;;;; of bytes is the error.
;;;;   - not "RIFF" [0], not "WAVE" [8];
;;;;   - the source ends inside those 12 bytes, before a data chunk, inside a
;;;;     chunk header, or inside a chunk before the data chunk (the fmt chunk
;;;;     too) [the source length];
;;;;   - a data chunk before any fmt chunk [the data chunk's offset];
;;;;   - fmt checks above [the fmt chunk's offset];
;;;;   - a data chunk whose SIZE runs past the end of the source [the data
;;;;     chunk's offset], inside a USE-PARTIAL-DATA restart: the buffer then
;;;;     holds the whole frames among the bytes that are there (a trailing
;;;;     partial frame is dropped, with the warning below);
;;;;   - a float32 sample that is a NaN or an infinity [its first byte], inside a
;;;;     USE-VALUE restart whose value, a real, replaces that sample (as a
;;;;     SINGLE-FLOAT; the next non-finite sample signals again).
;;;; Errors come in file order. Every proper prefix of a valid file that ends
;;;; before the end of its data chunk header fails at its own length; one that
;;;; ends inside the samples fails at the data chunk's offset (USE-PARTIAL-DATA).
;;;;
;;;; Warnings (WAV-FORMAT-WARNING, WARN, MUFFLE-WARNING):
;;;;   - "partial frame": leftover data bytes [the first leftover byte], once.
;;;;
;;;; (WRITE-WAV buffer destination &key (encoding :pcm16) (if-exists :supersede))
;;;;   DESTINATION is NIL (return a fresh (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))
;;;;   holding the file), a binary output stream of element type (UNSIGNED-BYTE
;;;;   8) (write to it and return the stream), or a pathname designator (write
;;;;   the file, opened with IF-EXISTS, and return DESTINATION). ENCODING is a
;;;;   keyword of +WAV-ENCODINGS+ (else TYPE-ERROR). WRITE-WAV checks and
;;;;   encodes the whole buffer before it writes a byte, so an error leaves a
;;;;   stream untouched and creates or changes no file.
;;;;
;;;; Layout written, with D = FRAMES * BLOCK-ALIGN, BLOCK-ALIGN = CHANNELS *
;;;; bytes per sample, P = 1 when D is odd (a 0 pad byte after the samples), else 0:
;;;;   PCM:   "RIFF" <36 + D + P> "WAVE"
;;;;          "fmt " <16> <1> <channels> <rate> <rate * block-align> <block-align> <bits>
;;;;          "data" <D> <samples> [pad]
;;;;   float: "RIFF" <50 + D> "WAVE"
;;;;          "fmt " <18> <3> <channels> <rate> <rate * block-align> <block-align> <32> <0>
;;;;          "fact" <4> <frames>
;;;;          "data" <D> <samples>
;;;; No other chunk is written, and never WAVE_FORMAT_EXTENSIBLE. The PCM
;;;; layout is the 44-byte header Python's wave module and libsndfile write, so
;;;; their 16-bit files (and libsndfile's plain 24-bit files) read and write
;;;; back byte for byte.
;;;;
;;;; Encoding a sample x:
;;;;   PCM, B bits: the code is round(x * 2^(B-1)) (CL ROUND: halves to even),
;;;;     clamped to -2^(B-1) .. 2^(B-1)-1. So a sample read from a B-bit file
;;;;     writes back to its own code, 1.0 writes the top code, and samples above
;;;;     1 in magnitude clip.
;;;;   float32: x itself.
;;;; Before writing: a NaN or infinite sample signals AUDIO-ENCODE-ERROR (INDEX
;;;; its index in the samples vector, the first one); then, for PCM, when any
;;;; sample has |x| > 1, one SAMPLES-CLIPPED warning (COUNT those samples, PEAK
;;;; the largest |x|), with MUFFLE-WARNING. 1.0 itself is not clipped.
;;;; A file whose RIFF size would not fit 32 bits (#xFFFFFFFF), or whose
;;;; bytes-per-second field wouldn't, signals AUDIO-LIMIT-EXCEEDED (LIMIT
;;;; #xFFFFFFFF, VALUE the size or the field) before writing.

(in-package #:audio-protocol)

(defparameter +wav-encodings+ '(:pcm16 :pcm24 :float32)
  "The encodings READ-WAV reads and WRITE-WAV writes. Data.")

(defun read-wav (source)
  "Read a RIFF/WAVE file from SOURCE (an octet vector, a binary input stream or
a pathname designator). Return (values pcm-buffer encoding). Signals
AUDIO-FORMAT-ERROR (with USE-PARTIAL-DATA or USE-VALUE where wav.lisp says) and
WAV-FORMAT-WARNING."
  (declare (ignorable source))
  (%au0-pending 'read-wav))

(defun write-wav (buffer destination &key (encoding :pcm16) (if-exists :supersede))
  "Write BUFFER as a RIFF/WAVE file in ENCODING to DESTINATION (NIL for a fresh
octet vector, a binary output stream, or a pathname designator opened with
IF-EXISTS). Signals AUDIO-ENCODE-ERROR or AUDIO-LIMIT-EXCEEDED before writing
anything, and SAMPLES-CLIPPED for PCM samples beyond -1..1."
  (declare (ignorable buffer destination encoding if-exists))
  (%au0-pending 'write-wav))
