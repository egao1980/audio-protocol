;;;; AU0 live cross-read: soundfile 0.12.1 (and wave, for 16-bit files) reads
;;;; what WRITE-WAV wrote. Out of process; skipped when uv is absent.

(in-package #:audio-protocol/tests)

(defun expected-code (x encoding)
  "The code wav.lisp says WRITE-WAV writes for the sample X."
  (ecase encoding
    (:float32 (f32-bits x))
    ((:pcm16 :pcm24)
     (let ((half (expt 2 (1- (encoding-bits encoding)))))
       (max (- half) (min (1- half) (round (* (rational x) half))))))))

(defun random-live-buffer (channels)
  "A buffer of random samples in -1.2..1.2, some on PCM codes, some between."
  (let* ((frames (rnd-pick 0 1 7 (rnd 400)))
         (rate (rnd-pick 8000 11025 22050 44100 48000 96000)))
    (make-pcm-buffer :sample-rate rate :channels channels
                     :samples (loop repeat (* frames channels)
                                    collect (if (zerop (rnd 2))
                                                (/ (- (rnd 65536) 32768) 32768)
                                                (/ (- (rnd 2400001) 1200000) 1000000))))))

(defun rnd-pick (&rest choices) (nth (rnd (length choices)) choices))

(deftest soundfile-reads-what-we-write
  (if (not (uv-available-p))
      (skip "uv not found: the live soundfile cross-read needs uv on PATH")
      (with-temp-dir (dir)
        (let* ((*seed* 20261124)
               (cases (loop for i from 0
                            for (encoding channels) in '((:pcm16 1) (:pcm16 2) (:pcm16 3)
                                                         (:pcm24 1) (:pcm24 2) (:pcm24 3)
                                                         (:float32 1) (:float32 2) (:float32 3))
                            nconc (loop for j below 4
                                        collect (list (format nil "~2,'0D-~(~A~)-~D.wav"
                                                              (+ (* 4 i) j) encoding channels)
                                                      encoding
                                                      (random-live-buffer channels))))))
          (handler-bind ((samples-clipped #'muffle-warning))
            (loop for (name encoding b) in cases
                  do (write-wav b (merge-pathnames name dir) :encoding encoding)))
          (multiple-value-bind (code out err)
              (stack-process:run (list "uv" "run" "--no-project"
                                       "--with" "soundfile==0.12.1" "--with" "numpy==2.1.3"
                                       "python" "-I"
                                       (namestring (fixture-path "generate-au0.py"))
                                       "--dump" (namestring dir))
                                 :output :string :error-output :string :timeout 600)
            (ok (and (integerp code) (zerop code)) err)
            (let* ((entries (with-standard-io-syntax
                              (let ((*read-eval* nil)
                                    (*package* (find-package '#:audio-protocol/tests)))
                                (read-from-string out))))
                   (bad (loop for (name encoding b) in cases
                              for entry = (assoc name entries :test #'string=)
                              unless (and entry
                                          (equal (list encoding (pcm-buffer-sample-rate b)
                                                       (pcm-buffer-channels b)
                                                       (pcm-buffer-frames b)
                                                       (map 'list (lambda (x)
                                                                    (expected-code x encoding))
                                                            (pcm-buffer-samples b)))
                                                 (rest entry)))
                                collect name)))
              (ok (= (length cases) (length entries)))
              (ok (null bad) (report bad))))))))
