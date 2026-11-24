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

(defparameter +wav-fmt-codes+ '((:pcm16 1 16) (:pcm24 1 24) (:float32 3 32))
  "(encoding format-tag bits) for each of +WAV-ENCODINGS+: fmt check 4 when
reading, the fmt chunk when writing. Data.")

(defparameter +wav-guid-suffix+
  (coerce '(#x00 #x00 #x00 #x00 #x10 #x00 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71)
          '(simple-array (unsigned-byte 8) (*)))
  "The last 14 bytes of every standard WAVE_FORMAT_EXTENSIBLE sub-format GUID.")

(defconstant +wav-block-samples+ 4096
  "Samples per binary-protocol PACK-INTO or UNPACK call. UNPACK scans its whole
buffer on every call (binary-protocol#4), so each call gets a small fresh slice.")

(defconstant +wav-read-block+ 65536
  "Bytes per READ-SEQUENCE on a stream, so a huge declared size allocates only
the bytes that are there.")

(defun %read-restart-value (what)
  "For a restart's :INTERACTIVE: read a form for WHAT from *QUERY-IO*, evaluate
it, and return its value in a list."
  (format *query-io* "~&Form for the ~A (evaluated): " what)
  (force-output *query-io*)
  (list (eval (read *query-io*))))

(defun %input-stream-p (x)
  (and (streamp x) (input-stream-p x)))

(defun %output-stream-p (x)
  (and (streamp x) (output-stream-p x)))

(defun %sample-width (encoding)
  "Bytes per sample of ENCODING."
  (ecase encoding (:pcm16 2) (:pcm24 3) (:float32 4)))

(defun %repeat-format (code count)
  "A little-endian binary-protocol format of COUNT copies of CODE: \"<4096h\",
or \"<HbHb...\" for the two-letter 24-bit code (binary-protocol#3: the low u16,
then the signed high byte)."
  (if (= 1 (length code))
      (format nil "<~D~A" count code)
      (with-output-to-string (s)
        (write-char #\< s)
        (loop repeat count do (write-string code s)))))

(defstruct (%wav-reader (:constructor %make-wav-reader (octets stream))
                        (:copier nil)
                        (:predicate nil))
  "Where READ-WAV reads: an octet vector or a binary input stream, and POS, the
bytes consumed since the call. Every offset READ-WAV reports counts from there."
  (octets nil :read-only t)
  (stream nil :read-only t)
  (pos 0 :type (integer 0)))

(defun %take (reader n)
  "A fresh (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) of the next N bytes, fewer when
the source ends first. No byte past them is read. A stream is read in blocks,
so a huge N allocates only the bytes that are there."
  (let ((octets (%wav-reader-octets reader))
        (pos (%wav-reader-pos reader)))
    (if octets
        (let* ((end (min (length octets) (+ pos n))) ; LENGTH respects a fill pointer
               (out (make-array (- end pos) :element-type '(unsigned-byte 8))))
          (replace out octets :start2 pos :end2 end)
          (setf (%wav-reader-pos reader) end)
          out)
        (let ((stream (%wav-reader-stream reader))
              (blocks '())
              (got 0))
          (loop while (< got n)
                do (let* ((want (min +wav-read-block+ (- n got)))
                          (block (make-array want :element-type '(unsigned-byte 8)))
                          (k (read-sequence block stream)))
                     (incf got k)
                     (push (if (< k want) (subseq block 0 k) block) blocks)
                     (when (< k want)
                       (return))))
          (incf (%wav-reader-pos reader) got)
          (if (and blocks (null (rest blocks)))
              (first blocks)
              (let ((out (make-array got :element-type '(unsigned-byte 8)))
                    (start 0))
                (dolist (block (nreverse blocks) out)
                  (replace out block :start1 start)
                  (incf start (length block)))))))))

(defun %skip (reader n)
  "Move past the next N bytes, or to the end of the source when it comes first.
A stream is read through one scratch block."
  (let ((octets (%wav-reader-octets reader)))
    (if octets
        (setf (%wav-reader-pos reader)
              (min (length octets) (+ (%wav-reader-pos reader) n)))
        (let ((scratch (make-array (min n +wav-read-block+) :element-type '(unsigned-byte 8)))
              (left n))
          (loop while (plusp left)
                do (let* ((want (min left (length scratch)))
                          (k (read-sequence scratch (%wav-reader-stream reader) :end want)))
                     (incf (%wav-reader-pos reader) k)
                     (decf left k)
                     (when (< k want)
                       (return))))))))

(defun %need (reader n what &optional empty-reason)
  "The next N bytes, or AUDIO-FORMAT-ERROR at the source length (POS after the
short read): the source ends inside WHAT, or EMPTY-REASON when it had no byte left."
  (let ((bytes (%take reader n)))
    (when (< (length bytes) n)
      (error 'audio-format-error
             :offset (%wav-reader-pos reader)
             :reason (if (and empty-reason (zerop (length bytes)))
                         empty-reason
                         (format nil "the source ends inside ~A" what))))
    bytes))

(defun %skip-exactly (reader n what)
  "Skip the next N bytes, or signal AUDIO-FORMAT-ERROR at the source length."
  (let ((start (%wav-reader-pos reader)))
    (%skip reader n)
    (when (< (- (%wav-reader-pos reader) start) n)
      (error 'audio-format-error
             :offset (%wav-reader-pos reader)
             :reason (format nil "the source ends inside ~A" what)))))

(defun %id= (octets id)
  "Do the 4 OCTETS spell the chunk id ID?"
  (every (lambda (byte char) (= byte (char-code char))) octets id))

(defun %parse-fmt (body size offset)
  "Checks 1-6 of the file header, in order, on BODY, the first (min SIZE 40)
bytes of the fmt chunk at OFFSET. Returns (encoding channels rate block-align)."
  (when (< size 16)
    (error 'audio-format-error :offset offset :reason "a fmt chunk shorter than 16 bytes"))
  (destructuring-bind (tag channels rate bytes-per-second block-align bits)
      (stack-binary:unpack "<HHIIHH" (subseq body 0 16))
    (declare (ignore bytes-per-second))
    (flet ((unsupported (format-tag reason)
             (error 'wav-unsupported-encoding
                    :offset offset :format-tag format-tag :bits bits :reason reason)))
      (unless (member tag '(1 3 #xFFFE))
        (unsupported tag "not PCM, IEEE float or WAVE_FORMAT_EXTENSIBLE"))
      (when (= tag #xFFFE)
        ;; OR: CB-SIZE (bytes 16 and 17) is read only when all 40 bytes are there.
        (when (or (< size 40) (< (first (stack-binary:unpack "<H" (subseq body 16 18))) 22))
          (error 'audio-format-error
                 :offset offset
                 :reason "a WAVE_FORMAT_EXTENSIBLE fmt chunk too short for its fields"))
        (unless (equalp (subseq body 26 40) +wav-guid-suffix+)
          (unsupported #xFFFE "a non-standard sub-format GUID"))
        (destructuring-bind (valid-bits sub-tag) (stack-binary:unpack "<H4xH" (subseq body 18 26))
          (unless (or (zerop valid-bits) (= valid-bits bits))
            (unsupported sub-tag (format nil "~D valid bits of ~D" valid-bits bits)))
          (setf tag sub-tag)))
      (let ((encoding (first (find-if (lambda (entry)
                                        (and (= tag (second entry)) (= bits (third entry))))
                                      +wav-fmt-codes+))))
        (unless encoding
          (unsupported tag "not 16-bit or 24-bit PCM, or 32-bit IEEE float"))
        (when (or (zerop channels) (zerop rate))
          (error 'audio-format-error
                 :offset offset
                 :reason (if (zerop channels) "no channels" "a sample rate of 0")))
        (unless (= block-align (* channels (/ bits 8)))
          (error 'audio-format-error
                 :offset offset
                 :reason (format nil "block align ~D, not ~D channel~:P of ~D bytes"
                                 block-align channels (/ bits 8))))
        (list encoding channels rate block-align)))))

(defun %non-finite-sample (offset)
  "Signal the AUDIO-FORMAT-ERROR for the non-finite float32 sample at OFFSET,
inside a USE-VALUE restart; return the value, a real, as a SINGLE-FLOAT."
  (restart-case (error 'audio-format-error
                       :offset offset :reason "a NaN or infinite float32 sample")
    (use-value (value)
      :report "Use a real in place of this sample."
      :interactive (lambda () (%read-restart-value "sample (a real)"))
      (check-type value real)
      (coerce value 'single-float))))

(defun %decode-float32-block (samples start slice n data-start)
  "Decode the N float32 samples of SLICE, our own copy, into SAMPLES from
START. A non-finite sample, found on its bits, signals in file order, and its
bytes are zeroed before decoding: no NaN or infinity is ever made."
  (let ((replacements '()))
    (loop for bits in (stack-binary:unpack (%repeat-format "I" n) slice)
          for k from 0
          when (= #xFF (ldb (byte 8 23) bits))
            do (push (cons (+ start k) (%non-finite-sample (+ data-start (* 4 (+ start k)))))
                     replacements)
               (fill slice 0 :start (* 4 k) :end (* 4 (1+ k))))
    (loop for x in (stack-binary:unpack (%repeat-format "f" n) slice)
          for i from start
          do (setf (aref samples i) x))
    (loop for (i . x) in replacements
          do (setf (aref samples i) x))))

(defun %decode-samples (encoding bytes count data-start)
  "A fresh SAMPLE-VECTOR of the first COUNT samples of BYTES, each code decoded
exactly. DATA-START is the source offset of BYTES."
  (let ((samples (%sample-vector count))
        (width (%sample-width encoding)))
    (loop for start from 0 below count by +wav-block-samples+
          do (let* ((n (min +wav-block-samples+ (- count start)))
                    (slice (subseq bytes (* start width) (* (+ start n) width))))
               (ecase encoding
                 (:pcm16
                  (loop for code in (stack-binary:unpack (%repeat-format "h" n) slice)
                        for i from start
                        do (setf (aref samples i) (/ (float code 1f0) 32768))))
                 (:pcm24
                  (loop for (low high) on (stack-binary:unpack (%repeat-format "Hb" n) slice)
                          by #'cddr
                        for i from start
                        do (setf (aref samples i)
                                 (/ (float (+ low (* 65536 high)) 1f0) 8388608))))
                 (:float32
                  (%decode-float32-block samples start slice n data-start)))))
    samples))

(defun %read-data (reader fmt offset size)
  "The buffer and encoding of the data chunk at OFFSET, declared SIZE bytes."
  (destructuring-bind (encoding channels rate block-align) fmt
    (let ((bytes (%take reader size))) ; at most SIZE bytes: never the pad byte
      (when (< (length bytes) size)
        (restart-case (error 'audio-format-error
                             :offset offset
                             :reason (format nil "the data chunk declares ~D bytes and ~D are there"
                                             size (length bytes)))
          (use-partial-data ()
            :report "Read the whole frames that are there."
            nil)))
      (let* ((frames (floor (length bytes) block-align))
             (used (* frames block-align))
             (samples (%decode-samples encoding bytes (* frames channels) (+ offset 8))))
        (when (< used (length bytes))
          (warn 'wav-format-warning :offset (+ offset 8 used) :reason "partial frame"))
        (values (%make-pcm-buffer rate channels frames samples) encoding)))))

(defun %read-wav (reader)
  (unless (%id= (%need reader 4 "the RIFF header") "RIFF")
    (error 'audio-format-error :offset 0 :reason "not a RIFF file"))
  (%need reader 4 "the RIFF header")    ; the RIFF size, not checked
  (unless (%id= (%need reader 4 "the RIFF header") "WAVE")
    (error 'audio-format-error :offset 8 :reason "not a WAVE file"))
  (let ((fmt nil))
    (loop
      (let ((offset (%wav-reader-pos reader)))
        (destructuring-bind (id size)
            (stack-binary:unpack "<4sI" (%need reader 8 "a chunk header"
                                               "the source ends before a data chunk"))
          (cond ((%id= id "data")
                 (unless fmt
                   (error 'audio-format-error
                          :offset offset :reason "a data chunk before the fmt chunk"))
                 ;; Nothing after the data chunk is read.
                 (return (%read-data reader fmt offset size)))
                ((and (null fmt) (%id= id "fmt "))
                 ;; At most 40 bytes are kept; the checks run once all SIZE are there.
                 (let ((body (%need reader (min size 40) "the fmt chunk")))
                   (%skip-exactly reader (- size (length body)) "the fmt chunk")
                   (setf fmt (%parse-fmt body size offset))
                   (%skip-exactly reader (logand size 1) "the fmt chunk's pad byte")))
                (t
                 (%skip-exactly reader (+ size (logand size 1)) "a chunk"))))))))

(defun read-wav (source)
  "Read a RIFF/WAVE file from SOURCE (an octet vector, a binary input stream or
a pathname designator). Return (values pcm-buffer encoding). Signals
AUDIO-FORMAT-ERROR (with USE-PARTIAL-DATA or USE-VALUE where wav.lisp says) and
WAV-FORMAT-WARNING."
  (check-type source (or (vector (unsigned-byte 8)) (satisfies %input-stream-p) pathname string))
  (etypecase source
    ((vector (unsigned-byte 8))
     (%read-wav (%make-wav-reader source nil)))
    (stream
     (%read-wav (%make-wav-reader nil source)))
    ((or pathname string)
     (with-open-file (in source :element-type '(unsigned-byte 8))
       (%read-wav (%make-wav-reader nil in))))))

(defun %non-finite-p (x)
  "Is the SINGLE-FLOAT X a NaN or an infinity? Decided without comparing X: SBCL
traps a comparison with a NaN."
  #+sbcl (or (sb-ext:float-nan-p x) (sb-ext:float-infinity-p x))
  #-sbcl (= #xFF (ldb (byte 8 23) (first (stack-binary:unpack "<I" (stack-binary:pack "<f" x))))))

(defun %check-wav-field (value limit what)
  "AUDIO-LIMIT-EXCEEDED when VALUE doesn't fit a WAV field whose largest value is LIMIT."
  (when (> value limit)
    (error 'audio-limit-exceeded
           :limit limit :value value
           :reason (format nil "the ~A doesn't fit its WAV field" what))))

(defun %warn-if-clipped (samples)
  "One SAMPLES-CLIPPED warning when some of the (finite) SAMPLES has |x| > 1."
  (let ((count 0)
        (peak 0f0))
    (loop for x across samples
          do (let ((a (abs x)))
               (when (> a 1f0)
                 (incf count)
                 (setf peak (max peak a)))))
    (when (plusp count)
      (warn 'samples-clipped :count count :peak peak
                             :reason "PCM codes clamped to the encoding's range"))))

(defun %pcm-code (x half)
  "round(X * HALF), halves to even, clamped to -HALF .. HALF-1. Computed in
DOUBLE-FLOAT, where a single-float times a power of two is exact; in single
float, 3.4e38 * 2^15 would overflow."
  (max (- half) (min (1- half) (round (* (float x 1d0) half)))))

(defun %encode-wav (samples encoding tag bits channels rate frames block-align data pad)
  "The whole file in one fresh octet vector: the header of the file header's
layout, the samples, and the 0 pad byte when PAD is 1."
  (let* ((float-p (= tag 3))
         (header (if float-p 58 44))
         (width (/ bits 8))
         (count (length samples))
         (out (make-array (+ header data pad) :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
    (stack-binary:pack-into
     out
     (if float-p "<4sI4s4sIHHIIHHH4sII4sI" "<4sI4s4sIHHIIHH4sI")
     (if float-p
         (list "RIFF" (+ 50 data) "WAVE"
               "fmt " 18 3 channels rate (* rate block-align) block-align 32 0
               "fact" 4 frames
               "data" data)
         (list "RIFF" (+ 36 data pad) "WAVE"
               "fmt " 16 1 channels rate (* rate block-align) block-align bits
               "data" data)))
    (loop for start from 0 below count by +wav-block-samples+
          do (let* ((end (min count (+ start +wav-block-samples+)))
                    (n (- end start)))
               (stack-binary:pack-into
                out
                (ecase encoding
                  (:pcm16 (%repeat-format "h" n))
                  (:pcm24 (%repeat-format "Hb" n))
                  (:float32 (%repeat-format "f" n)))
                (ecase encoding
                  (:pcm16
                   (loop for i from start below end
                         collect (%pcm-code (aref samples i) 32768)))
                  (:pcm24
                   (loop for i from start below end
                         for code = (%pcm-code (aref samples i) 8388608)
                         collect (ldb (byte 16 0) code)
                         collect (ash code -16)))
                  (:float32
                   (loop for i from start below end
                         collect (aref samples i))))
                :start (+ header (* start width)))))
    out))

(defun write-wav (buffer destination &key (encoding :pcm16) (if-exists :supersede))
  "Write BUFFER as a RIFF/WAVE file in ENCODING to DESTINATION (NIL for a fresh
octet vector, a binary output stream, or a pathname designator opened with
IF-EXISTS). Signals AUDIO-ENCODE-ERROR or AUDIO-LIMIT-EXCEEDED before writing
anything, and SAMPLES-CLIPPED for PCM samples beyond -1..1."
  (check-type buffer pcm-buffer)
  (check-type destination (or null pathname string (satisfies %output-stream-p)))
  (check-type encoding (member :pcm16 :pcm24 :float32)) ; +WAV-ENCODINGS+
  (let* ((samples (pcm-buffer-samples buffer))
         (bad (position-if #'%non-finite-p samples)))
    (when bad
      (error 'audio-encode-error :index bad :reason "a NaN or infinite sample"))
    (destructuring-bind (tag bits) (rest (assoc encoding +wav-fmt-codes+))
      (let* ((channels (pcm-buffer-channels buffer))
             (rate (pcm-buffer-sample-rate buffer))
             (frames (pcm-buffer-frames buffer))
             (block-align (* channels (/ bits 8)))
             (data (* frames block-align))
             (pad (logand data 1)))
        ;; Every field fits before binary-protocol packs it, so none of its
        ;; conditions (or their restarts) reach the caller.
        (%check-wav-field block-align #xFFFF "block align")
        (%check-wav-field (* rate block-align) #xFFFFFFFF "bytes per second")
        (%check-wav-field (if (= tag 3) (+ 50 data) (+ 36 data pad)) #xFFFFFFFF "RIFF size")
        (when (= tag 1)
          (%warn-if-clipped samples))
        (let ((octets (%encode-wav samples encoding tag bits channels rate frames
                                   block-align data pad)))
          (etypecase destination
            (null octets)
            (stream
             (write-sequence octets destination)
             destination)
            ((or pathname string)
             (with-open-file (out destination :direction :output
                                              :element-type '(unsigned-byte 8)
                                              :if-exists if-exists
                                              :if-does-not-exist :create)
               ;; OUT is NIL when IF-EXISTS is NIL and the file exists.
               (when out
                 (write-sequence octets out)))
             destination)))))))
