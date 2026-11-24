;;;; AU0 WAV: the layout written, reading it back, encoding rules, chunks,
;;;; WAVE_FORMAT_EXTENSIBLE, sources and destinations.

(in-package #:audio-protocol/tests)

;;; Layout written ---------------------------------------------------------------

(defparameter +layout-rows+
  '((:pcm16 8000 1 (0 1 -1 32767 -32768))
    (:pcm16 44100 2 (100 -100 2000 -2000 0 0))
    (:pcm16 22050 1 ())
    (:pcm24 48000 1 (0 1 -1 8388607 -8388608))
    (:pcm24 96000 2 (5 -5 70000 -70000))
    (:pcm24 8000 3 (1 2 3))
    (:float32 16000 1 (0 #x3F800000 #xBF800000 #x3FC00000))
    (:float32 8000 2 (#x3F000000 #xBE800000)))
  "(encoding rate channels codes): odd PCM24 data (pad byte), empty data, 3 channels.")

(deftest-parametrize writes-the-documented-layout
    ((encoding rate channels codes) :rows +layout-rows+)
  (ok (equalp (canonical-wav encoding rate channels codes)
              (write-wav (codes-buffer encoding rate channels codes) nil :encoding encoding))))

(deftest header-fields-spelled-out
  ;; One PCM16 stereo frame at 8000 Hz: 44 header bytes, then 4 data bytes.
  (ok (equalp (octets "RIFF" 40 0 0 0 "WAVE"
                      "fmt " 16 0 0 0 1 0 2 0 #x40 #x1F 0 0 #x00 #x7D 0 0 4 0 16 0
                      "data" 4 0 0 0 #x00 #x40 #x00 #xC0)
              (write-wav (buffer 8000 2 1/2 -1/2) nil)))
  ;; One PCM24 mono frame: odd data, so a pad byte that the RIFF size counts.
  (ok (equalp (octets "RIFF" 40 0 0 0 "WAVE"
                      "fmt " 16 0 0 0 1 0 1 0 #x40 #x1F 0 0 #xC0 #x5D 0 0 3 0 24 0
                      "data" 3 0 0 0 #x00 #x00 #x40 0)
              (write-wav (buffer 8000 1 1/2) nil :encoding :pcm24)))
  ;; float32: an 18-byte fmt chunk (cbSize 0) and a fact chunk with the frames.
  (ok (equalp (octets "RIFF" 54 0 0 0 "WAVE"
                      "fmt " 18 0 0 0 3 0 1 0 #x40 #x1F 0 0 #x00 #x7D 0 0 4 0 32 0 0 0
                      "fact" 4 0 0 0 1 0 0 0
                      "data" 4 0 0 0 #x00 #x00 #xC0 #x3F)
              (write-wav (buffer 8000 1 1.5) nil :encoding :float32))))

(deftest default-encoding-is-pcm16
  (let ((b (buffer 8000 1 0.25)))
    (ok (equalp (write-wav b nil :encoding :pcm16) (write-wav b nil)))))

;;; Reading the layout back -------------------------------------------------------

(deftest-parametrize reads-the-documented-layout
    ((encoding rate channels codes) :rows +layout-rows+)
  (multiple-value-bind (b got-encoding)
      (read-wav (canonical-wav encoding rate channels codes))
    (ok (eq encoding got-encoding))
    (ok (= rate (pcm-buffer-sample-rate b)))
    (ok (= channels (pcm-buffer-channels b)))
    (ok (= (floor (length codes) channels) (pcm-buffer-frames b)))
    (ok (typep (pcm-buffer-samples b) 'sample-vector))
    (ok (equal codes (buffer-codes b encoding)))))

(deftest-parametrize random-round-trips
    ((encoding channels) :rows '((:pcm16 1) (:pcm16 2) (:pcm24 1) (:pcm24 2) (:pcm24 5)
                                 (:float32 1) (:float32 2)))
  (let* ((*seed* (+ 7 channels (length (string encoding))))
         (bad '()))
    (dotimes (i 20)
      (let* ((frames (rnd 300))
             (codes (random-codes encoding (* frames channels)))
             (bytes (canonical-wav encoding 44100 channels codes))
             (b (read-wav bytes)))
        (unless (and (equal codes (buffer-codes b encoding))
                     (equalp bytes (write-wav b nil :encoding encoding)))
          (push i bad))))
    (ok (null bad) (report bad))))

(deftest float32-keeps-signed-zero-and-subnormals
  (let* ((codes (list #x80000000 0 1 #x807FFFFF #x7F7FFFFF))
         (bytes (canonical-wav :float32 8000 1 codes)))
    (ok (equal codes (buffer-codes (read-wav bytes) :float32)))
    (ok (equalp bytes (write-wav (read-wav bytes) nil :encoding :float32)))))

;;; Encoding a sample -------------------------------------------------------------

(defparameter +pcm16-encoding-rows+
  '((0 0) (1/32768 1) (-1/32768 -1) (1/65536 0) (3/65536 2) (-1/65536 0) (-3/65536 -2)
    (5/65536 2) (1/2 16384) (-1 -32768) (1 32767) (32767/32768 32767))
  "(sample code) for :PCM16: CL ROUND, halves to even; 1.0 writes the top code.")

(deftest-parametrize pcm16-codes
    ((x code) :rows +pcm16-encoding-rows+)
  (multiple-value-bind (bytes warnings)
      (call-collecting-warnings (lambda () (write-wav (buffer 8000 1 x) nil)))
    (ok (equalp (canonical-wav :pcm16 8000 1 (list code)) bytes))
    (ok (null warnings) "not clipped")))

(deftest-parametrize pcm24-codes
    ((x code) :rows '((1/8388608 1) (1/16777216 0) (3/16777216 2) (-1 -8388608)
                      (1 8388607) (1/2 4194304)))
  (ok (equalp (canonical-wav :pcm24 8000 1 (list code))
              (call-collecting-warnings
               (lambda () (write-wav (buffer 8000 1 x) nil :encoding :pcm24))))))

(deftest pcm-clips-with-one-warning
  (dolist (encoding '(:pcm16 :pcm24))
    (multiple-value-bind (bytes warnings)
        (call-collecting-warnings
         (lambda () (write-wav (buffer 8000 1 1.5 -3.0 0.5 1.0 -1.0 1.25) nil :encoding encoding)))
      (let ((top (1- (expt 2 (1- (encoding-bits encoding)))))
            (bottom (- (expt 2 (1- (encoding-bits encoding))))))
        (ok (equal (list top bottom (/ (1+ top) 2) top bottom top)
                   (buffer-codes (read-wav bytes) encoding)))
        (ok (= 1 (length warnings)))
        (ok (typep (first warnings) 'samples-clipped))
        (ok (= 3 (samples-clipped-count (first warnings))))
        (ok (= 3.0 (samples-clipped-peak (first warnings))))))))

(deftest float32-does-not-clip
  (multiple-value-bind (bytes warnings)
      (call-collecting-warnings (lambda () (write-wav (buffer 8000 1 1.5 -3.0) nil :encoding :float32)))
    (ok (null warnings))
    (ok (equal (list (f32-bits 1.5) (f32-bits -3.0)) (buffer-codes (read-wav bytes) :float32)))))

(deftest clipped-warning-can-be-muffled-or-escalated
  (ok (signals (handler-bind ((samples-clipped (lambda (w) (error "escalated ~A" w))))
                 (write-wav (buffer 8000 1 2.0) nil))
               'error))
  (let ((bytes (handler-bind ((samples-clipped #'muffle-warning))
                 (write-wav (buffer 8000 1 2.0) nil))))
    (ok (equalp (canonical-wav :pcm16 8000 1 '(32767)) bytes))))

#+sbcl
(deftest non-finite-samples-are-refused
  (let* ((nan (sb-kernel:make-single-float #x7FC00000))
         (inf sb-ext:single-float-positive-infinity)
         (with-nan (sb-int:with-float-traps-masked (:invalid)
                     (make-pcm-buffer :sample-rate 8000 :channels 2
                                      :samples (list 0.0 0.5 nan 0.25))))
         (with-inf (make-pcm-buffer :sample-rate 8000 :samples (list 0.0 inf))))
    (dolist (encoding +wav-encodings+)
      (let ((c (handler-case (progn (write-wav with-nan nil :encoding encoding) :none)
                 (audio-encode-error (c) c))))
        (ok (typep c 'audio-encode-error))
        (ok (and (typep c 'audio-encode-error) (= 2 (audio-encode-error-index c)))))
      (let ((c (handler-case (progn (write-wav with-inf nil :encoding encoding) :none)
                 (audio-encode-error (c) c))))
        (ok (and (typep c 'audio-encode-error) (= 1 (audio-encode-error-index c))))))
    (with-temp-dir (dir)
      (let ((path (merge-pathnames "nan.wav" dir)))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          (ok (signals (write-wav with-nan out) 'audio-encode-error)))
        (ok (zerop (length (file-octets path))) "nothing written to the stream")
        (ok (signals (write-wav with-nan (merge-pathnames "nan2.wav" dir)) 'audio-encode-error))
        (ok (null (probe-file (merge-pathnames "nan2.wav" dir))) "no file created")))))

(deftest size-fields-that-do-not-fit
  ;; bytes per second = rate * block align = #xFFFFFFFF * 4
  (let ((c (handler-case (progn (write-wav (make-pcm-buffer :sample-rate #xFFFFFFFF :channels 2
                                                            :frames 1)
                                           nil)
                                :none)
             (audio-limit-exceeded (c) c))))
    (ok (typep c 'audio-limit-exceeded))
    (ok (and (typep c 'audio-limit-exceeded) (= #xFFFFFFFF (audio-limit-exceeded-limit c))))))

(deftest write-argument-errors
  (ok (signals (write-wav (buffer 8000 1 0.0) nil :encoding :pcm8) 'type-error))
  (ok (signals (write-wav '(0.0) nil) 'type-error))
  (ok (signals (write-wav (buffer 8000 1 0.0) 42) 'type-error)))

;;; Chunks ------------------------------------------------------------------------

(defparameter +tone+ '(1000 -1000 2000 -2000 3000)
  "Five PCM16 mono codes used by the chunk tests.")

(defun tone-buffer (&optional (rate 8000)) (codes-buffer :pcm16 rate 1 +tone+))

(deftest-parametrize chunks-before-data-are-skipped
    ((label bytes)
     :rows (list
            (list "LIST INFO before fmt"
                  (riff (chunk "LIST" "INFO" (chunk "INAM" "Tune" 0) (chunk "IART" "Me" 0))
                        (fmt 1 1 8000 16) (data-chunk :pcm16 +tone+)))
            (list "LIST between fmt and data"
                  (riff (fmt 1 1 8000 16) (chunk "LIST" "INFO" (chunk "ICMT" "odd" 0))
                        (data-chunk :pcm16 +tone+)))
            (list "odd JUNK with its pad byte"
                  (riff (chunk "JUNK" 1 2 3) (fmt 1 1 8000 16) (data-chunk :pcm16 +tone+)))
            (list "fact, PEAK, bext, cue"
                  (riff (fmt 1 1 8000 16) (chunk "fact" (u32 5)) (chunk "PEAK" (u32 1) (u32 0))
                        (chunk "bext" (make-list 13 :initial-element 65))
                        (chunk "cue " (u32 0)) (data-chunk :pcm16 +tone+)))
            (list "an empty chunk" (riff (chunk "zero") (fmt 1 1 8000 16) (data-chunk :pcm16 +tone+)))
            (list "a second fmt chunk is ignored"
                  (riff (fmt 1 1 8000 16) (fmt 3 2 96000 32) (data-chunk :pcm16 +tone+)))
            (list "a fmt chunk of 18 bytes"
                  (riff (fmt 1 1 8000 16 :extra (u16 0)) (data-chunk :pcm16 +tone+)))
            (list "a fmt chunk with extra bytes"
                  (riff (fmt 1 1 8000 16 :extra '(0 0 9 9 9)) (data-chunk :pcm16 +tone+)))
            (list "a wrong bytes-per-second"
                  (riff (fmt 1 1 8000 16 :bytes-per-second 1) (data-chunk :pcm16 +tone+)))))
  (declare (ignorable label))
  (multiple-value-bind (b encoding warnings) (read-with-warnings bytes)
    (ok (eq :pcm16 encoding))
    (ok (equalp (tone-buffer) b))
    (ok (null warnings))))

(deftest-parametrize nothing-after-data-is-read
    ((label bytes)
     :rows (list
            (list "LIST after data"
                  (octets (riff (fmt 1 1 8000 16) (data-chunk :pcm16 +tone+)
                                (chunk "LIST" "INFO" (chunk "INAM" "x" 0)))))
            (list "trailing garbage" (octets (riff (fmt 1 1 8000 16) (data-chunk :pcm16 +tone+))
                                             "ID3" 4 0 0 0 0))
            (list "a second data chunk"
                  (riff (fmt 1 1 8000 16) (data-chunk :pcm16 +tone+) (data-chunk :pcm16 '(7 7))))
            (list "a chunk after data that runs past the end"
                  (octets (riff (fmt 1 1 8000 16) (data-chunk :pcm16 +tone+)) "LIST" (u32 9999)))))
  (declare (ignorable label))
  (ok (equalp (tone-buffer) (read-with-warnings bytes))))

(deftest-parametrize riff-size-is-not-checked
    ((size) :rows '(0 4 1000000 #xFFFFFFFF))
  (ok (equalp (tone-buffer)
              (read-wav (riff-sized size (fmt 1 1 8000 16) (data-chunk :pcm16 +tone+))))))

(deftest empty-data-chunk
  (let ((b (read-wav (riff (fmt 1 2 44100 16) (chunk "data")))))
    (ok (= 0 (pcm-buffer-frames b)))
    (ok (= 2 (pcm-buffer-channels b)))
    (ok (= 44100 (pcm-buffer-sample-rate b)))))

(deftest partial-frame-warning
  ;; 2 stereo PCM16 frames, then 3 leftover bytes: at 12 + 24 + 8 + 8 = 52.
  (multiple-value-bind (b encoding warnings)
      (read-with-warnings (riff (fmt 1 2 8000 16) (chunk "data" (sample-bytes :pcm16 '(1 2 3 4)) 9 9 9)))
    (declare (ignore encoding))
    (ok (equal '(1 2 3 4) (buffer-codes b :pcm16)))
    (ok (= 1 (length warnings)))
    (ok (typep (first warnings) 'wav-format-warning))
    (ok (and warnings (= 52 (wav-format-warning-offset (first warnings)))))))

(deftest a-warning-left-alone-does-not-stop-the-read
  (let ((*error-output* (make-broadcast-stream)))
    (ok (equal '(1) (buffer-codes (read-wav (riff (fmt 1 1 8000 16) (chunk "data" 1 0 9)))
                                  :pcm16)))))

;;; WAVE_FORMAT_EXTENSIBLE -----------------------------------------------------------

(deftest-parametrize extensible-sub-formats
    ((encoding channels codes)
     :rows '((:pcm16 2 (1 -1 300 -300)) (:pcm24 3 (1 2 3 -4 -5 -6)) (:float32 1 (#x3E800000))))
  (let* ((bits (encoding-bits encoding))
         (bytes (riff (fmt-ext (encoding-tag encoding) channels 48000 bits :mask 7)
                      (chunk "fact" (u32 1))
                      (data-chunk encoding codes))))
    (multiple-value-bind (b got) (read-wav bytes)
      (ok (eq encoding got))
      (ok (= channels (pcm-buffer-channels b)))
      (ok (equal codes (buffer-codes b encoding))))))

(deftest extensible-valid-bits-zero-is-accepted
  (ok (equal '(5) (buffer-codes (read-wav (riff (fmt-ext 1 1 8000 16 :valid-bits 0)
                                                (data-chunk :pcm16 '(5))))
                                :pcm16))))

;;; Sources and destinations -----------------------------------------------------------

(deftest read-from-every-source
  (let ((bytes (canonical-wav :pcm16 8000 1 +tone+)))
    (ok (equalp (tone-buffer) (read-wav bytes)) "simple octet vector")
    (let ((adjustable (make-array (length bytes) :element-type '(unsigned-byte 8)
                                                 :adjustable t :fill-pointer t)))
      (replace adjustable bytes)
      (ok (equalp (tone-buffer) (read-wav adjustable)) "non-simple octet vector"))
    (with-temp-dir (dir)
      (let ((path (merge-pathnames "tone.wav" dir)))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          (write-sequence bytes out))
        (ok (equalp (tone-buffer) (read-wav path)) "pathname")
        (ok (equalp (tone-buffer) (read-wav (namestring path))) "namestring")
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (ok (equalp (tone-buffer) (read-wav in)) "stream"))))))

(deftest stream-is-left-after-the-samples
  (let ((bytes (octets (riff (fmt 1 1 8000 16) (chunk "data" (sample-bytes :pcm16 '(1 2 3))))
                       "TAIL")))
    (with-temp-dir (dir)
      (let ((path (merge-pathnames "tail.wav" dir)))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          ;; Two bytes of something else first: offsets count from the stream's position.
          (write-sequence (octets 7 7) out)
          (write-sequence bytes out))
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (read-byte in) (read-byte in)
          (ok (equal '(1 2 3) (buffer-codes (read-wav in) :pcm16)))
          ;; The data chunk has 6 bytes (even): the next byte is the "T" of TAIL.
          (ok (= (char-code #\T) (read-byte in))))
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (read-byte in)
          (let ((c (format-failure (lambda () (read-wav in)))))
            (ok (typep c 'audio-format-error) "not RIFF from byte 1")
            (ok (and (typep c 'audio-format-error) (= 0 (audio-format-error-offset c))))))))))

(deftest odd-data-stream-stops-before-the-pad-byte
  (let ((bytes (octets (riff (fmt 1 1 8000 24) (data-chunk :pcm24 '(5))) "Z")))
    ;; RIFF adds the pad byte 0 after the 3 data bytes; then "Z".
    (with-temp-dir (dir)
      (let ((path (merge-pathnames "odd.wav" dir)))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          (write-sequence bytes out))
        (with-open-file (in path :element-type '(unsigned-byte 8))
          (ok (equal '(5) (buffer-codes (read-wav in) :pcm24)))
          (ok (= 0 (read-byte in)) "the pad byte is still there"))))))

(deftest write-to-every-destination
  (let* ((b (tone-buffer))
         (bytes (canonical-wav :pcm16 8000 1 +tone+)))
    (ok (equalp bytes (write-wav b nil)))
    (ok (typep (write-wav b nil) '(simple-array (unsigned-byte 8) (*))))
    (with-temp-dir (dir)
      (let ((path (merge-pathnames "out.wav" dir))
            (path2 (merge-pathnames "out2.wav" dir))
            (path3 (merge-pathnames "out3.wav" dir)))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          (ok (eq out (write-wav b out)) "a stream returns the stream"))
        (ok (equalp bytes (file-octets path)))
        (ok (equal path2 (write-wav b path2)) "a pathname returns it")
        (ok (equalp bytes (file-octets path2)))
        (ok (equal (namestring path3) (write-wav b (namestring path3))))
        (ok (equalp bytes (file-octets path3)))
        (ok (signals (write-wav b path2 :if-exists :error) 'file-error))
        (write-wav (buffer 8000 1 0.0) path2 :if-exists :supersede)
        (ok (equal '(0) (buffer-codes (read-wav path2) :pcm16)))))))

(deftest read-argument-errors
  (ok (signals (read-wav 42) 'type-error))
  (ok (signals (read-wav '(1 2 3)) 'type-error)))

(deftest reading-never-modifies-and-writing-never-mutates
  (let* ((b (buffer 8000 1 0.5 2.0 -0.25))
         (copy (buffer 8000 1 0.5 2.0 -0.25)))
    (handler-bind ((samples-clipped #'muffle-warning))
      (write-wav b nil))
    (ok (equalp copy b))))
