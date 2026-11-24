;;;; AU0 WAV errors: offsets, unsupported encodings, restarts, prefixes.

(in-package #:audio-protocol/tests)

(defparameter +fmt16+ (fmt 1 1 8000 16) "A mono PCM16 fmt chunk: 24 bytes, at 12 in a file.")

(defparameter +malformed-rows+
  (list
   (list "empty" (octets) 0)
   (list "three bytes" (octets "RIF") 3)
   (list "not RIFF" (octets "RIFX" (u32 4) "WAVE") 0)
   (list "not RIFF, short" (octets "RIFX") 0)
   (list "ends inside WAVE" (octets "RIFF" (u32 4) "WAV") 11)
   (list "not WAVE" (octets "RIFF" (u32 4) "AVI ") 8)
   (list "header only" (riff) 12)
   (list "fmt, no data" (riff +fmt16+) 36)
   (list "ends inside a chunk id" (octets (riff +fmt16+) "da") 38)
   (list "ends inside a chunk size" (octets (riff +fmt16+) "data" 4 0) 42)
   (list "ends inside fmt" (subseq (riff +fmt16+ (data-chunk :pcm16 +tone+)) 0 30) 30)
   (list "a chunk past the end, no data" (octets (riff +fmt16+) "JUNK" (u32 100) 1 2 3) 47)
   (list "data before fmt" (riff (data-chunk :pcm16 +tone+) +fmt16+) 12)
   (list "data, no fmt" (riff (data-chunk :pcm16 +tone+)) 12)
   (list "data before fmt after LIST"
         (riff (chunk "LIST" "INFO") (data-chunk :pcm16 +tone+) +fmt16+) 24)
   (list "fmt too short" (riff (chunk "fmt " (u16 1) (u16 1) (u32 8000) (u32 16000) (u16 2))
                               (data-chunk :pcm16 +tone+))
         12)
   (list "no channels" (riff (fmt 1 0 8000 16 :block-align 0) (data-chunk :pcm16 +tone+)) 12)
   (list "sample rate 0" (riff (fmt 1 1 0 16) (data-chunk :pcm16 +tone+)) 12)
   (list "block align too small" (riff (fmt 1 2 8000 16 :block-align 2) (data-chunk :pcm16 +tone+)) 12)
   (list "block align too large" (riff (fmt 1 1 8000 24 :block-align 4) (data-chunk :pcm16 +tone+)) 12)
   (list "bad fmt after LIST"
         (riff (chunk "LIST" "INFO" (chunk "INAM" "abc")) (fmt 1 1 0 16) (data-chunk :pcm16 +tone+))
         36)
   (list "extensible fmt of 18 bytes"
         (riff (fmt #xFFFE 1 8000 16 :extra (u16 0)) (data-chunk :pcm16 +tone+)) 12)
   (list "extensible cbSize too small"
         (riff (fmt-ext 1 1 8000 16 :cb-size 10) (data-chunk :pcm16 +tone+)) 12))
  "(label bytes offset): an AUDIO-FORMAT-ERROR at OFFSET (wav.lisp, \"Errors\").")

(deftest-parametrize malformed-offsets
    ((label bytes offset) :rows +malformed-rows+)
  (declare (ignorable label))
  (ok (eql offset (failure-offset bytes))))

(deftest-parametrize malformed-is-not-unsupported
    ((label bytes offset) :rows +malformed-rows+)
  (declare (ignorable label offset))
  (let ((c (format-failure (lambda () (read-wav bytes)))))
    (ok (typep c 'audio-format-error))
    (ng (typep c 'wav-unsupported-encoding))
    (ok (and (typep c 'audio-format-error) (stringp (audio-error-reason c))) "has a reason")))

(deftest-parametrize malformed-offers-no-restart
    ((label bytes offset) :rows +malformed-rows+)
  (declare (ignorable label offset))
  (ok (equal '() (restarts-offered (lambda () (read-wav bytes)) 'audio-format-error
                                   '(audio-protocol:use-partial-data use-value)))))

(defparameter +unsupported-rows+
  (list
   (list "8-bit PCM" (fmt 1 1 8000 8) 1 8)
   (list "32-bit PCM" (fmt 1 1 8000 32) 1 32)
   (list "20-bit PCM" (fmt 1 1 8000 20 :block-align 3) 1 20)
   (list "float64" (fmt 3 1 8000 64) 3 64)
   (list "float16" (fmt 3 1 8000 16) 3 16)
   (list "A-law" (fmt 6 1 8000 8) 6 8)
   (list "mu-law" (fmt 7 1 8000 8) 7 8)
   (list "IMA ADPCM" (fmt #x11 1 8000 4 :block-align 256 :extra (list (u16 2) (u16 505))) #x11 4)
   (list "MPEG layer 3" (fmt #x55 2 44100 0 :block-align 1 :extra (list (u16 12) (make-list 12 :initial-element 0)))
         #x55 0)
   (list "extensible 8-bit PCM" (fmt-ext 1 1 8000 8) 1 8)
   (list "extensible float64" (fmt-ext 3 1 8000 64) 3 64)
   (list "extensible with a non-standard GUID"
         (fmt-ext 1 1 8000 16 :suffix '(1 2 3 4 5 6 7 8 9 10 11 12 13 14)) #xFFFE 16)
   (list "extensible with 20 valid bits of 24" (fmt-ext 1 1 8000 24 :valid-bits 20) 1 24))
  "(label fmt-chunk format-tag bits): WAV-UNSUPPORTED-ENCODING.")

(deftest-parametrize unsupported-encodings
    ((label fmt-chunk tag bits) :rows +unsupported-rows+)
  (declare (ignorable label))
  (let ((c (format-failure (lambda () (read-wav (riff fmt-chunk (chunk "data" 0 0 0 0)))))))
    (ok (typep c 'wav-unsupported-encoding))
    (ok (and (typep c 'wav-unsupported-encoding) (eql tag (wav-unsupported-encoding-format-tag c))))
    (ok (and (typep c 'wav-unsupported-encoding) (eql bits (wav-unsupported-encoding-bits c))))
    (ok (and (typep c 'wav-unsupported-encoding) (eql 12 (audio-format-error-offset c))))))

(deftest unsupported-before-other-fmt-checks
  ;; Check 2 (the tag) comes before checks 5 and 6 (channels, block align).
  (ok (typep (format-failure (lambda () (read-wav (riff (fmt 6 0 0 8 :block-align 7)
                                                         (chunk "data")))))
             'wav-unsupported-encoding)))

;;; A data chunk past the end: USE-PARTIAL-DATA ------------------------------------

(defun short-data (available)
  "A stereo PCM16 file whose data chunk declares 16 bytes (4 frames) but holds
only the first AVAILABLE. The data chunk is at 36; its samples start at 44."
  (octets (riff (fmt 1 2 8000 16))
          "data" (u32 16)
          (subseq (octets (sample-bytes :pcm16 '(1 2 3 4 5 6 7 8))) 0 available)))

(deftest short-data-signals-at-the-data-chunk
  (ok (eql 36 (failure-offset (short-data 10))))
  (ok (eql 36 (failure-offset (short-data 0))))
  (ok (equal '(audio-protocol:use-partial-data)
             (restarts-offered (lambda () (read-wav (short-data 10))) 'audio-format-error
                               '(audio-protocol:use-partial-data use-value)))))

(deftest-parametrize use-partial-data-keeps-whole-frames
    ((available frames warning-offset)
     :rows '((10 2 52) (8 2 nil) (3 0 44) (0 0 nil) (15 3 56)))
  (let ((warnings '()))
    (let ((b (handler-bind ((audio-warning (lambda (w) (push w warnings) (muffle-warning w)))
                            (audio-format-error #'use-partial-data))
               (read-wav (short-data available)))))
      (ok (= frames (pcm-buffer-frames b)))
      (ok (equal (subseq '(1 2 3 4 5 6 7 8) 0 (* 2 frames)) (buffer-codes b :pcm16)))
      (if warning-offset
          (ok (and (= 1 (length warnings))
                   (eql warning-offset (wav-format-warning-offset (first warnings)))))
          (ok (null warnings))))))

;;; Non-finite float32 samples: USE-VALUE ---------------------------------------------

(defun float-file (codes)
  "A mono float32 file with a 16-byte fmt chunk: samples from byte 44."
  (riff (fmt 3 1 8000 32) (data-chunk :float32 codes)))

(defparameter +non-finite+ '(#x7FC00000 #x7F800000 #xFF800000 #xFFC00001 #x7F800001)
  "Quiet NaN, +inf, -inf, a negative NaN, a signalling NaN.")

(deftest-parametrize non-finite-float-samples
    ((bits) :rows +non-finite+)
  (let ((bytes (float-file (list #x3F800000 bits 0))))
    (ok (eql 48 (failure-offset bytes)))
    (ok (equal '(use-value)
               (restarts-offered (lambda () (read-wav bytes)) 'audio-format-error
                                 '(audio-protocol:use-partial-data use-value))))
    (ok (equal (list (f32-bits 1.0) (f32-bits 0.25) 0)
               (buffer-codes (invoking 'audio-format-error 'use-value
                                       (lambda () (read-wav bytes)) 1/4)
                             :float32)))))

(deftest every-non-finite-sample-signals-in-order
  (let ((offsets '()))
    (let ((b (handler-bind ((audio-format-error
                              (lambda (c)
                                (push (audio-format-error-offset c) offsets)
                                (use-value (- (length offsets)) c))))
               (read-wav (float-file (list #x7FC00000 #x3F000000 #x7F800000 #xFF800000))))))
      (ok (equal '(44 52 56) (reverse offsets)))
      (ok (equal (list (f32-bits -1) (f32-bits 0.5) (f32-bits -2) (f32-bits -3))
                 (buffer-codes b :float32))))))

;;; Prefixes -----------------------------------------------------------------------

(deftest every-prefix-fails-where-it-ends
  (let* ((file (riff (chunk "LIST" "INFO" (chunk "INAM" "Prefix test"))
                     (fmt 1 2 8000 16)
                     (data-chunk :pcm16 '(1 -1 2 -2 3 -3))))
         (data (- (length file) 8 12))   ; the data chunk's offset
         (bad '()))
    (dotimes (n (length file))
      (let ((prefix (subseq file 0 n)))
        (if (< n (+ data 8))
            (unless (eql n (failure-offset prefix)) (push n bad))
            (let ((b (handler-bind ((audio-format-error #'use-partial-data))
                       (read-with-warnings prefix))))
              (unless (and (eql data (failure-offset prefix))
                           (= (floor (- n data 8) 4) (pcm-buffer-frames b)))
                (push n bad))))))
    (ok (null bad) (report (reverse bad)))))
