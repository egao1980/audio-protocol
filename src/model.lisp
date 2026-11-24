;;;; AU0 model: PCM buffers, stems, frames and seconds, gains in dB.
;;;;
;;;; A PCM-BUFFER is a sample rate (frames per second), a channel count, a
;;;; frame count and the samples: a (SIMPLE-ARRAY SINGLE-FLOAT (*)) of
;;;; FRAMES * CHANNELS elements, interleaved (frame 0 channel 0, frame 0
;;;; channel 1, ..., frame 1 channel 0, ...), as WAV stores them.
;;;;
;;;; Why SINGLE-FLOAT. Every sample of a 16-bit or 24-bit PCM file is an
;;;; integer code c with |c| <= 2^23, and c / 2^(bits-1) is exact in a
;;;; SINGLE-FLOAT (a 24-bit significand, and the scale is a power of two), as is
;;;; every float32 sample. So reading a file and writing it back in its own
;;;; encoding is lossless, while gains, pans, sums and resampling get floating
;;;; point without an intermediate integer clip. Integer buffers would need a
;;;; second representation for every float operation; doubles would double the
;;;; memory for no gain at these word lengths. FluidSynth renders floats.
;;;;
;;;; Range. Samples are nominally in -1..1: a PCM code c is c / 2^(bits-1), so
;;;; 16-bit audio spans -1 .. 32767/32768. A buffer may hold samples beyond 1 in
;;;; magnitude (a sum before clipping, a float32 file); MIX's clipping policy
;;;; and WRITE-WAV's PCM encoding deal with them (SAMPLES-CLIPPED). Samples are
;;;; finite: no function here makes a NaN or an infinity from finite input,
;;;; READ-WAV doesn't return one, and WRITE-WAV refuses one.
;;;;
;;;; Buffers compare with EQUALP (sample by sample, with =). The slots are
;;;; read-only; the samples vector is the creator's to fill (a synth writes into
;;;; a fresh buffer), but no function of this package modifies a buffer it is
;;;; given.
;;;;
;;;; Time. A time in seconds is an exact rational where it can be.
;;;; (SECONDS-TO-FRAME s rate) is floor(s * rate + 1/2), s taken as the exact
;;;; RATIONAL of a float: the frame nearest to s, halves rounding up.
;;;; PCM-BUFFER-DURATION is FRAMES / SAMPLE-RATE.

(in-package #:audio-protocol)

(deftype sample-vector ()
  "The samples of a PCM-BUFFER, and of one channel (PCM-CHANNEL)."
  '(simple-array single-float (*)))

(defun %sample-vector (n)
  (make-array n :element-type 'single-float :initial-element 0.0f0))

(defstruct (pcm-buffer (:constructor %make-pcm-buffer (sample-rate channels frames samples))
                       (:copier nil))
  "Interleaved SINGLE-FLOAT PCM (see the file header). Make one with MAKE-PCM-BUFFER."
  (sample-rate 44100 :type (integer 1 #xFFFFFFFF) :read-only t)
  (channels 1 :type (integer 1 65535) :read-only t)
  (frames 0 :type (integer 0) :read-only t)
  (samples (%sample-vector 0) :type sample-vector :read-only t))

(defmethod print-object ((buffer pcm-buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity t)
    (format stream "~D Hz, ~D channel~:P, ~D frame~:P"
            (pcm-buffer-sample-rate buffer) (pcm-buffer-channels buffer)
            (pcm-buffer-frames buffer))))

(defun make-pcm-buffer (&key (sample-rate 44100) (channels 1) frames samples)
  "A fresh PCM-BUFFER. With SAMPLES NIL, FRAMES (default 0) frames of silence.
With SAMPLES, a sequence of reals, a copy of them as SINGLE-FLOATs, interleaved;
its length must be a multiple of CHANNELS, and FRAMES, when given, the
quotient. SAMPLE-RATE is an integer 1..#xFFFFFFFF, CHANNELS 1..65535 (else
TYPE-ERROR); a length that doesn't fit signals AUDIO-ERROR."
  (check-type sample-rate (integer 1 #xFFFFFFFF))
  (check-type channels (integer 1 65535))
  (check-type frames (or null (integer 0)))
  (check-type samples (or null sequence))
  (if (null samples)
      (let ((frames (or frames 0)))
        (%make-pcm-buffer sample-rate channels frames (%sample-vector (* frames channels))))
      (let ((n (length samples)))
        (multiple-value-bind (count rest) (floor n channels)
          (unless (zerop rest)
            (error 'audio-error
                   :reason (format nil "~D samples are not whole frames of ~D channel~:P"
                                   n channels)))
          (when (and frames (/= frames count))
            (error 'audio-error
                   :reason (format nil "~D samples are ~D frames, not ~D" n count frames)))
          (let ((vector (%sample-vector n)))
            (map-into vector (lambda (x) (coerce x 'single-float)) samples)
            (%make-pcm-buffer sample-rate channels count vector))))))

(defun pcm-buffer-duration (buffer)
  "FRAMES / SAMPLE-RATE: the exact duration of BUFFER in seconds."
  (check-type buffer pcm-buffer)
  (/ (pcm-buffer-frames buffer) (pcm-buffer-sample-rate buffer)))

(defun sample-ref (buffer frame channel)
  "The sample of BUFFER at FRAME (0..frames-1) and CHANNEL (0..channels-1)."
  (check-type buffer pcm-buffer)
  (let ((channels (pcm-buffer-channels buffer)))
    (unless (and (typep frame '(integer 0)) (< frame (pcm-buffer-frames buffer))
                 (typep channel '(integer 0)) (< channel channels))
      (error 'audio-error
             :reason (format nil "no sample at frame ~S, channel ~S" frame channel)))
    (aref (pcm-buffer-samples buffer) (+ (* frame channels) channel))))

(defun pcm-channel (buffer channel)
  "A fresh SAMPLE-VECTOR holding channel CHANNEL of BUFFER, one sample per frame."
  (check-type buffer pcm-buffer)
  (let ((channels (pcm-buffer-channels buffer))
        (frames (pcm-buffer-frames buffer))
        (samples (pcm-buffer-samples buffer)))
    (unless (and (typep channel '(integer 0)) (< channel channels))
      (error 'audio-error :reason (format nil "no channel ~S" channel)))
    (let ((out (%sample-vector frames)))
      (dotimes (i frames out)
        (setf (aref out i) (aref samples (+ (* i channels) channel)))))))

(defun pcm-from-channels (channels &key (sample-rate 44100))
  "A fresh PCM-BUFFER whose channel k holds the reals of the k-th sequence of
CHANNELS (a non-empty list of sequences of one length; else AUDIO-ERROR)."
  (check-type channels cons)
  (let ((frames (length (first channels)))
        (count (length channels)))
    (unless (every (lambda (c) (= (length c) frames)) channels)
      (error 'audio-error :reason "channels of different lengths"))
    (let ((out (%sample-vector (* frames count))))
      (loop for c in channels
            for k from 0
            do (let ((i k))
                 (map nil (lambda (x)
                            (setf (aref out i) (coerce x 'single-float))
                            (incf i count))
                      c)))
      (make-pcm-buffer :sample-rate sample-rate :channels count :samples out))))

(defun seconds-to-frame (seconds sample-rate)
  "floor(SECONDS * SAMPLE-RATE + 1/2): the frame nearest SECONDS (halves round
up), SECONDS a non-negative real taken as its exact RATIONAL."
  (check-type seconds (real 0))
  (check-type sample-rate (integer 1))
  (values (floor (+ (* (rational seconds) sample-rate) 1/2))))

(defun db-to-gain (db)
  "10^(DB/20) as a DOUBLE-FLOAT: 0 dB is exactly 1, -6.02 dB about 1/2."
  (check-type db real)
  (expt 10d0 (/ (float db 1d0) 20d0)))

(defun note-frequency (key)
  "Equal-tempered frequency of MIDI key KEY (0..127) in Hz, as a DOUBLE-FLOAT:
440 * 2^((KEY - 69) / 12). Key 69 (A4) is exactly 440, key 60 (C4) about 261.63."
  (check-type key (integer 0 127))
  (* 440d0 (expt 2d0 (/ (- key 69) 12d0))))

;;; Stems ------------------------------------------------------------------------

(defstruct (stem (:copier nil))
  "A named buffer: one track or one channel of a render (RENDER-STEMS), or any
labelled input of MIX. TRACK is the SMF track index of a stem by track, CHANNEL
the MIDI channel (0..15) of a stem by channel; NIL otherwise."
  (name "" :type string :read-only t)
  (buffer (make-pcm-buffer) :type pcm-buffer :read-only t)
  (track nil :type (or null (integer 0)) :read-only t)
  (channel nil :type (or null (integer 0 15)) :read-only t))
