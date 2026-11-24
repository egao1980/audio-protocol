;;;; Conditions and restarts (CLHS 9).
;;;;
;;;; audio-error (error)                      REASON
;;;; ├── audio-format-error (+ parse-error)   OFFSET: bytes READ-WAV can't read
;;;; │   └── wav-unsupported-encoding         FORMAT-TAG, BITS
;;;; ├── audio-encode-error                   INDEX: WRITE-WAV met a non-finite sample
;;;; ├── audio-mismatch-error                 INDEX: a MIX input of another rate or layout
;;;; ├── audio-limit-exceeded                 LIMIT, VALUE: a duration or size bound
;;;; └── audio-engine-error                   ENGINE: an engine broke the protocol
;;;;     └── audio-backend-unavailable        the engine can't run here
;;;; audio-warning (warning)                  REASON
;;;; ├── samples-clipped                      COUNT, PEAK
;;;; └── wav-format-warning                   OFFSET
;;;;
;;;; Backends signal these types; they don't define a parallel tree. Argument
;;;; type problems are TYPE-ERRORs from CHECK-TYPE (STORE-VALUE); an argument of
;;;; the right type but a wrong value (a region past the end of a buffer, list
;;;; lengths that disagree) is a plain AUDIO-ERROR with a REASON.
;;;;
;;;; Restarts, established at the site that can recover:
;;;;
;;;;   USE-PARTIAL-DATA  READ-WAV, around the AUDIO-FORMAT-ERROR for a data chunk
;;;;                     that runs past the end of the source: read the whole
;;;;                     frames that are there (wav.lisp).
;;;;   USE-VALUE         READ-WAV, around the AUDIO-FORMAT-ERROR for a
;;;;                     non-finite float32 sample: the value replaces it
;;;;                     (wav.lisp). MIX, around AUDIO-MISMATCH-ERROR: the value,
;;;;                     a PCM-BUFFER or STEM, replaces the input (mix.lisp).
;;;;   SKIP-INPUT        MIX, around AUDIO-MISMATCH-ERROR: leave the input out.
;;;;   CONTINUE          RENDER-MIDI and RENDER-STEMS, around
;;;;                     AUDIO-LIMIT-EXCEEDED for a render longer than
;;;;                     *MAX-RENDER-SECONDS*: render anyway (engine.lisp).
;;;;   MUFFLE-WARNING    around every AUDIO-WARNING (WARN establishes it).
;;;;
;;;; The functions USE-PARTIAL-DATA and SKIP-INPUT invoke the restart of that
;;;; name and return NIL when there is none, as CONTINUE and USE-VALUE do.

(in-package #:audio-protocol)

(define-condition audio-error (error)
  ((reason :initarg :reason :initform nil :reader audio-error-reason
           :documentation "A short lower-case explanation, e.g. \"no data chunk\"."))
  (:documentation "Root of every error audio-protocol and its backends signal.")
  (:report (lambda (c s)
             (format s "Audio error~@[: ~A~]" (audio-error-reason c)))))

(define-condition audio-format-error (audio-error parse-error)
  ((offset :initarg :offset :initform nil :reader audio-format-error-offset
           :documentation "Zero-based byte offset from the start of the source (for a
stream, from its position when the reader was called); wav.lisp lists which
byte each error points at."))
  (:documentation "The bytes are not audio the reader can read.")
  (:report (lambda (c s)
             (format s "Audio format error~@[ at byte ~D~]~@[: ~A~]"
                     (audio-format-error-offset c)
                     (audio-error-reason c)))))

(define-condition wav-unsupported-encoding (audio-format-error)
  ((format-tag :initarg :format-tag :initform nil
               :reader wav-unsupported-encoding-format-tag
               :documentation "The format tag of the fmt chunk; for WAVE_FORMAT_EXTENSIBLE,
the tag of its sub-format when the GUID is a standard one, else #xFFFE.")
   (bits :initarg :bits :initform nil :reader wav-unsupported-encoding-bits
         :documentation "Bits per sample of the fmt chunk."))
  (:documentation "A well-formed fmt chunk whose encoding isn't one of +WAV-ENCODINGS+.")
  (:report (lambda (c s)
             (format s "Unsupported WAV encoding: format tag ~@[#x~4,'0X~], ~@[~D bits~]~@[ (~A)~]"
                     (wav-unsupported-encoding-format-tag c)
                     (wav-unsupported-encoding-bits c)
                     (audio-error-reason c)))))

(define-condition audio-encode-error (audio-error)
  ((index :initarg :index :initform nil :reader audio-encode-error-index
          :documentation "Index into the samples vector of the first bad sample."))
  (:documentation "A buffer can't be encoded (a NaN or infinite sample).")
  (:report (lambda (c s)
             (format s "Audio encode error~@[ at sample ~D~]~@[: ~A~]"
                     (audio-encode-error-index c)
                     (audio-error-reason c)))))

(define-condition audio-mismatch-error (audio-error)
  ((index :initarg :index :initform nil :reader audio-mismatch-error-index
          :documentation "Zero-based position of the input in MIX's INPUTS."))
  (:documentation "An input doesn't fit the others: another sample rate, or a
channel count MIX can't pan.")
  (:report (lambda (c s)
             (format s "Audio input ~@[~D ~]doesn't match~@[: ~A~]"
                     (audio-mismatch-error-index c)
                     (audio-error-reason c)))))

(define-condition audio-limit-exceeded (audio-error)
  ((limit :initarg :limit :initform nil :reader audio-limit-exceeded-limit
          :documentation "The bound (seconds, bytes, ...).")
   (value :initarg :value :initform nil :reader audio-limit-exceeded-value
          :documentation "What was asked for, in the same unit."))
  (:documentation "A duration or size beyond a bound.")
  (:report (lambda (c s)
             (format s "Audio limit exceeded: ~A > ~A~@[ (~A)~]"
                     (audio-limit-exceeded-value c)
                     (audio-limit-exceeded-limit c)
                     (audio-error-reason c)))))

(define-condition audio-engine-error (audio-error)
  ((engine :initarg :engine :initform nil :reader audio-engine-error-engine
           :documentation "The AUDIO-ENGINE."))
  (:documentation "An engine failed, or returned something the protocol doesn't allow.")
  (:report (lambda (c s)
             (format s "Audio engine ~S failed~@[: ~A~]"
                     (audio-engine-error-engine c)
                     (audio-error-reason c)))))

(define-condition audio-backend-unavailable (audio-engine-error)
  ()
  (:documentation "The engine can't run here: no method for it, its native library
is missing, or a resource it needs (a SoundFont) isn't there.")
  (:report (lambda (c s)
             (format s "Audio engine ~S unavailable~@[: ~A~]"
                     (audio-engine-error-engine c)
                     (audio-error-reason c)))))

(define-condition audio-warning (warning)
  ((reason :initarg :reason :initform nil :reader audio-warning-reason
           :documentation "A short lower-case explanation."))
  (:documentation "Root of the warnings audio-protocol signals.")
  (:report (lambda (c s)
             (format s "Audio warning~@[: ~A~]" (audio-warning-reason c)))))

(define-condition samples-clipped (audio-warning)
  ((count :initarg :count :initform 0 :reader samples-clipped-count
          :documentation "How many samples had a magnitude above 1.")
   (peak :initarg :peak :initform nil :reader samples-clipped-peak
         :documentation "The largest magnitude, a SINGLE-FLOAT above 1."))
  (:documentation "Samples beyond -1..1 were clipped (MIX :CLIP, WRITE-WAV to PCM).")
  (:report (lambda (c s)
             (format s "~D sample~:P clipped, peak ~A~@[ (~A)~]"
                     (samples-clipped-count c)
                     (samples-clipped-peak c)
                     (audio-warning-reason c)))))

(define-condition wav-format-warning (audio-warning)
  ((offset :initarg :offset :initform nil :reader wav-format-warning-offset
           :documentation "Byte offset, counted as for AUDIO-FORMAT-ERROR-OFFSET."))
  (:documentation "READ-WAV read past something that loses data (wav.lisp, \"Warnings\").")
  (:report (lambda (c s)
             (format s "WAV format warning~@[ at byte ~D~]~@[: ~A~]"
                     (wav-format-warning-offset c)
                     (audio-warning-reason c)))))

(defun use-partial-data (&optional condition)
  "Invoke the USE-PARTIAL-DATA restart (READ-WAV, data chunk past the end).
Return NIL when there is no such restart."
  (let ((restart (find-restart 'use-partial-data condition)))
    (when restart (invoke-restart restart))))

(defun skip-input (&optional condition)
  "Invoke the SKIP-INPUT restart (MIX, an input that doesn't match).
Return NIL when there is no such restart."
  (let ((restart (find-restart 'skip-input condition)))
    (when restart (invoke-restart restart))))
