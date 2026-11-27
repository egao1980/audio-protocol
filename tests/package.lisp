(defpackage #:audio-protocol/tests
  (:use #:cl #:rove #:audio-protocol)
  ;; The AU3 tests call the DSP package as a consumer would, through a local
  ;; nickname: its RMS, PEAK, ... don't meet this package's own helpers.
  (:local-nicknames (#:dsp #:audio-protocol/dsp)))

(in-package #:audio-protocol/tests)

;;; Bytes ----------------------------------------------------------------------

(defun octets (&rest parts)
  "An octet vector from PARTS: integers 0..255, strings (one byte per
character) and lists or vectors of those, flattened in order."
  (let ((out (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (labels ((add (x)
               (etypecase x
                 (integer (vector-push-extend x out))
                 (string (loop for ch across x do (vector-push-extend (char-code ch) out)))
                 (sequence (map nil #'add x)))))
      (map nil #'add parts))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun le (n width)
  "N (signed or unsigned) as WIDTH little-endian bytes, two's complement."
  (loop for k below width collect (ldb (byte 8 (* 8 k)) n)))

(defun u16 (n) (le n 2))
(defun u32 (n) (le n 4))

(defun f32-bits (x)
  "The IEEE single bits of the real X, as an unsigned integer."
  (first (stack-binary:unpack "<I" (stack-binary:pack "<f" (coerce x 'single-float)))))

(defun bits-f32 (bits)
  "The SINGLE-FLOAT whose IEEE bits are BITS (finite values only)."
  (first (stack-binary:unpack "<f" (octets (u32 bits)))))

(defun chunk (id &rest body)
  "A RIFF chunk: ID, the size of BODY, BODY, and a 0 pad byte when that is odd."
  (let ((bytes (apply #'octets body)))
    (octets id (u32 (length bytes)) bytes (if (oddp (length bytes)) '(0) '()))))

(defun riff (&rest chunks)
  "\"RIFF\" <size> \"WAVE\" CHUNKS, the size counting \"WAVE\" and the chunks."
  (let ((body (apply #'octets chunks)))
    (octets "RIFF" (u32 (+ 4 (length body))) "WAVE" body)))

(defun riff-sized (size &rest chunks)
  "As RIFF, with the size field SIZE whatever the chunks are."
  (octets "RIFF" (u32 size) "WAVE" (apply #'octets chunks)))

(defun fmt (tag channels rate bits &key block-align bytes-per-second extra)
  "A fmt chunk. BLOCK-ALIGN and BYTES-PER-SECOND default to the consistent
values; EXTRA bytes follow the 16 standard ones."
  (let ((align (or block-align (* channels (ceiling bits 8)))))
    (chunk "fmt " (u16 tag) (u16 channels) (u32 rate)
           (u32 (or bytes-per-second (* rate align))) (u16 align) (u16 bits)
           (or extra '()))))

(defparameter +guid-suffix+ '(#x00 #x00 #x00 #x00 #x10 #x00 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71)
  "The last 14 bytes of every standard KSDATAFORMAT sub-format GUID.")

(defun fmt-ext (sub-tag channels rate bits &key (valid-bits bits) (suffix +guid-suffix+)
                                                (cb-size 22) (mask 0))
  "A WAVE_FORMAT_EXTENSIBLE fmt chunk (40 bytes) for SUB-TAG (1 PCM, 3 float)."
  (fmt #xFFFE channels rate bits
       :extra (list (u16 cb-size) (u16 valid-bits) (u32 mask) (u16 sub-tag) suffix)))

(defparameter +encoding-layout+
  '((:pcm16 1 16) (:pcm24 1 24) (:float32 3 32))
  "(encoding format-tag bits).")

(defun encoding-tag (encoding) (second (assoc encoding +encoding-layout+)))
(defun encoding-bits (encoding) (third (assoc encoding +encoding-layout+)))

(defun sample-bytes (encoding codes)
  "The data bytes of CODES (integers; IEEE bits for :FLOAT32)."
  (let ((width (/ (encoding-bits encoding) 8)))
    (loop for c in codes collect (le c width))))

(defun data-chunk (encoding codes)
  (chunk "data" (sample-bytes encoding codes)))

(defun canonical-wav (encoding rate channels codes)
  "The bytes WRITE-WAV writes for CODES (wav.lisp, \"Layout written\")."
  (let* ((bits (encoding-bits encoding))
         (frames (floor (length codes) channels)))
    (if (eq encoding :float32)
        (riff (fmt 3 channels rate 32 :extra (u16 0))
              (chunk "fact" (u32 frames))
              (data-chunk encoding codes))
        (riff (fmt 1 channels rate bits) (data-chunk encoding codes)))))

(defun code-sample (encoding code)
  "The SINGLE-FLOAT a code decodes to."
  (ecase encoding
    (:pcm16 (/ (coerce code 'single-float) 32768))
    (:pcm24 (/ (coerce code 'single-float) 8388608))
    (:float32 (bits-f32 code))))

(defun codes-buffer (encoding rate channels codes)
  "The buffer READ-WAV returns for CODES."
  (make-pcm-buffer :sample-rate rate :channels channels
                   :samples (mapcar (lambda (c) (code-sample encoding c)) codes)))

(defun buffer-codes (buffer encoding)
  "BUFFER's samples as codes: exact integers for PCM (each sample must be one),
IEEE bits for :FLOAT32."
  (map 'list (lambda (x)
               (ecase encoding
                 (:pcm16 (let ((c (* (rational x) 32768))) (assert (integerp c)) c))
                 (:pcm24 (let ((c (* (rational x) 8388608))) (assert (integerp c)) c))
                 (:float32 (f32-bits x))))
       (pcm-buffer-samples buffer)))

(defun samples (buffer)
  "BUFFER's samples as a list."
  (coerce (pcm-buffer-samples buffer) 'list))

(defun buffer (rate channels &rest samples)
  (make-pcm-buffer :sample-rate rate :channels channels :samples samples))

;;; Fixtures -------------------------------------------------------------------

(defun fixture-path (name)
  (asdf:system-relative-pathname "audio-protocol/tests" (format nil "tests/fixtures/~A" name)))

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun fixture-octets (name) (file-octets (fixture-path name)))

(defun read-sexp-file (path)
  (with-open-file (in path :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* (find-package '#:audio-protocol/tests)))
        (read in)))))

(defparameter *fixtures* (read-sexp-file (fixture-path "au0-files.sexp"))
  "(name encoding rate channels frames codes) for each .wav fixture.")

(defun fixture-entry (name)
  (or (assoc name *fixtures* :test #'string=) (error "no fixture ~A" name)))

(defmacro with-temp-dir ((var) &body body)
  (let ((dir (gensym "DIR")))
    `(stack-pathlib:with-temp-directory (,dir)
       (let ((,var (uiop:ensure-directory-pathname (stack-pathlib:path-pathname ,dir))))
         ,@body))))

;;; Conditions -----------------------------------------------------------------

(defun format-failure (thunk)
  "The AUDIO-FORMAT-ERROR THUNK signals (warnings muffled), or :NONE when it
returns. Other errors propagate, so a stub fails the test."
  (handler-case (handler-bind ((audio-warning #'muffle-warning))
                  (funcall thunk)
                  :none)
    (audio-format-error (c) c)))

(defun failure-offset (bytes)
  "The offset of the AUDIO-FORMAT-ERROR reading BYTES, or :NONE."
  (let ((c (format-failure (lambda () (read-wav bytes)))))
    (if (eq c :none) :none (audio-format-error-offset c))))

(defun read-with-warnings (source)
  "(values buffer encoding warnings) reading SOURCE, collecting and muffling
AUDIO-WARNINGs."
  (let ((warnings '()))
    (multiple-value-bind (buffer encoding)
        (handler-bind ((audio-warning (lambda (w) (push w warnings) (muffle-warning w))))
          (read-wav source))
      (values buffer encoding (nreverse warnings)))))

(defun call-collecting-warnings (thunk)
  "(values result warnings): THUNK's first value and the AUDIO-WARNINGs it
signalled, muffled."
  (let ((warnings '()))
    (values (handler-bind ((audio-warning (lambda (w) (push w warnings) (muffle-warning w))))
              (funcall thunk))
            (nreverse warnings))))

(defun restarts-offered (thunk condition-type names)
  "The NAMES of restarts visible at the first CONDITION-TYPE THUNK signals,
sorted; :NONE when it signals none. Warnings are muffled."
  (block nil
    (handler-bind ((audio-warning #'muffle-warning)
                   (condition (lambda (c)
                                (when (typep c condition-type)
                                  (return (sort (loop for name in names
                                                      when (find-restart name c) collect name)
                                                #'string< :key #'symbol-name))))))
      (funcall thunk)
      :none)))

(defun invoking (condition-type restart thunk &rest args)
  "Call THUNK, answering every CONDITION-TYPE with RESTART (a symbol) and
ARGS; warnings muffled. Returns THUNK's values."
  (handler-bind ((audio-warning #'muffle-warning)
                 (condition (lambda (c)
                              (when (typep c condition-type)
                                (let ((r (find-restart restart c)))
                                  (when r (apply #'invoke-restart r args)))))))
    (funcall thunk)))

;;; Numbers --------------------------------------------------------------------

(defun approx= (a b &optional (tolerance 1d-6))
  (<= (abs (- a b)) tolerance))

(defun every-approx= (xs ys &optional (tolerance 1d-6))
  (and (= (length xs) (length ys))
       (every (lambda (x y) (approx= x y tolerance)) xs ys)))

(defun single (x) (coerce x 'single-float))

(defun dpi () (coerce pi 'double-float))

;;; Signal analysis (test helpers, independent of the code under test) --------

(defun zero-crossing-frequency (vector start end rate)
  "Frequency estimate of the tone in VECTOR[START, END): sign changes / 2 per second."
  (let ((changes 0))
    (loop for i from (1+ start) below end
          when (or (and (minusp (aref vector (1- i))) (plusp (aref vector i)))
                   (and (plusp (aref vector (1- i))) (minusp (aref vector i))))
            do (incf changes))
    (/ changes 2d0 (/ (- end start) rate))))

(defun goertzel-power (vector start end rate frequency)
  "Power of VECTOR[START, END) at FREQUENCY (Goertzel)."
  (let* ((w (/ (* 2 (dpi) frequency) rate))
         (coeff (* 2 (cos w)))
         (s1 0d0) (s2 0d0))
    (loop for i from start below end
          do (let ((s (+ (aref vector i) (* coeff s1) (- s2))))
               (setf s2 s1 s1 s)))
    (- (+ (* s1 s1) (* s2 s2)) (* coeff s1 s2))))

(defun rms (vector &optional (start 0) (end (length vector)))
  (sqrt (/ (loop for i from start below end sum (expt (float (aref vector i) 1d0) 2))
           (max 1 (- end start)))))

(defun sine-vector (frames rate frequency &key (amplitude 0.5d0))
  (let ((v (make-array frames :element-type 'single-float)))
    (dotimes (i frames v)
      (setf (aref v i) (single (* amplitude (sin (/ (* 2 (dpi) frequency i) rate))))))))

;;; Deterministic pseudo-random numbers (a 31-bit LCG, the same on every Lisp) ---

(defvar *seed* 1)

(defun rnd (n)
  "An integer in [0, N)."
  (setf *seed* (mod (+ (* *seed* 1103515245) 12345) (expt 2 31)))
  (mod (ash *seed* -8) n))

(defun random-codes (encoding n)
  "N random codes of ENCODING (finite float32 values in -2..2 as IEEE bits)."
  (loop repeat n
        collect (ecase encoding
                  (:pcm16 (- (rnd 65536) 32768))
                  (:pcm24 (- (rnd 16777216) 8388608))
                  (:float32 (f32-bits (/ (- (rnd 2000001) 1000000) 500000))))))

;;; External tools -------------------------------------------------------------

(defun uv-available-p ()
  (handler-case (zerop (stack-process:run '("uv" "--version") :output nil :error-output nil
                                                              :timeout 60))
    (error () nil)))

;;; Reporting ------------------------------------------------------------------

(defun report (bad)
  (format nil "~D mismatches~@[, first: ~S~]" (length bad)
          (and bad (subseq bad 0 (min 5 (length bad))))))
