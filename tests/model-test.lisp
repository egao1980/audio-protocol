;;;; AU0 model (given): buffers, frames and seconds, gains, stems, conditions.

(in-package #:audio-protocol/tests)

(deftest make-pcm-buffer-shapes
  (let ((b (make-pcm-buffer :sample-rate 8000 :channels 2 :frames 3)))
    (ok (= 8000 (pcm-buffer-sample-rate b)))
    (ok (= 2 (pcm-buffer-channels b)))
    (ok (= 3 (pcm-buffer-frames b)))
    (ok (typep (pcm-buffer-samples b) 'sample-vector))
    (ok (equalp #(0.0 0.0 0.0 0.0 0.0 0.0) (pcm-buffer-samples b))))
  (let ((b (make-pcm-buffer :sample-rate 16000 :channels 2 :samples '(1 -1/2 0.25d0 0))))
    (ok (= 2 (pcm-buffer-frames b)))
    (ok (equalp #(1.0 -0.5 0.25 0.0) (pcm-buffer-samples b)))
    (ok (typep (pcm-buffer-samples b) 'sample-vector)))
  (let ((b (make-pcm-buffer)))
    (ok (= 44100 (pcm-buffer-sample-rate b)))
    (ok (= 1 (pcm-buffer-channels b)))
    (ok (zerop (pcm-buffer-frames b)))))

(deftest make-pcm-buffer-copies-its-samples
  (let* ((v (make-array 2 :element-type 'single-float :initial-contents '(0.5 0.25)))
         (b (make-pcm-buffer :samples v)))
    (setf (aref v 0) 0.0)
    (ok (= 0.5 (aref (pcm-buffer-samples b) 0)))))

(deftest make-pcm-buffer-errors
  (ok (signals (make-pcm-buffer :channels 2 :samples '(1 2 3)) 'audio-error))
  (ok (signals (make-pcm-buffer :channels 1 :frames 2 :samples '(1 2 3)) 'audio-error))
  (ok (signals (make-pcm-buffer :sample-rate 0) 'type-error))
  (ok (signals (make-pcm-buffer :channels 0) 'type-error))
  (ok (signals (make-pcm-buffer :channels 70000) 'type-error))
  (ok (signals (make-pcm-buffer :frames -1) 'type-error)))

(deftest buffers-compare-with-equalp
  (ok (equalp (buffer 8000 1 0.5 0.25) (buffer 8000 1 1/2 1/4)))
  (ng (equalp (buffer 8000 1 0.5 0.25) (buffer 8001 1 0.5 0.25)))
  (ng (equalp (buffer 8000 2 0.5 0.25) (buffer 8000 1 0.5 0.25))))

(deftest buffer-time
  (ok (= 1/2 (pcm-buffer-duration (make-pcm-buffer :sample-rate 8000 :frames 4000))))
  (ok (= 3/44100 (pcm-buffer-duration (make-pcm-buffer :frames 3))))
  (ok (= 0 (seconds-to-frame 0 8000)))
  (ok (= 4000 (seconds-to-frame 1/2 8000)))
  (ok (= 1 (seconds-to-frame 1/16000 8000)) "a half frame rounds up")
  (ok (= 0 (seconds-to-frame 1/16001 8000)))
  (ok (= 4000 (seconds-to-frame 0.5d0 8000)))
  (ok (= 22050 (seconds-to-frame 1/2 44100))))

(deftest sample-access
  (let ((b (buffer 8000 2 0.1 0.2 0.3 0.4 0.5 0.6)))
    (ok (= (single 0.3) (sample-ref b 1 0)))
    (ok (= (single 0.6) (sample-ref b 2 1)))
    (ok (signals (sample-ref b 3 0) 'audio-error))
    (ok (signals (sample-ref b 0 2) 'audio-error))
    (ok (equalp (map 'vector #'single '(0.2 0.4 0.6)) (pcm-channel b 1)))
    (ok (typep (pcm-channel b 0) 'sample-vector))
    (ok (signals (pcm-channel b 2) 'audio-error))
    (ok (equalp b (pcm-from-channels (list (pcm-channel b 0) (pcm-channel b 1))
                                     :sample-rate 8000)))
    (ok (signals (pcm-from-channels (list '(1 2) '(1))) 'audio-error))))

(deftest gains-and-frequencies
  (ok (= 1d0 (db-to-gain 0)))
  (ok (approx= 0.5d0 (db-to-gain -6.0206d0) 1d-5))
  (ok (approx= 10d0 (db-to-gain 20) 1d-12))
  (ok (typep (db-to-gain -3) 'double-float))
  (ok (= 440d0 (note-frequency 69)))
  (ok (= 880d0 (note-frequency 81)))
  (ok (approx= 261.6255653005986d0 (note-frequency 60) 1d-9))
  (ok (signals (note-frequency 128) 'type-error)))

(deftest stems
  (let* ((b (buffer 8000 1 0.5))
         (s (make-stem :name "Alto" :buffer b :track 2)))
    (ok (stem-p s))
    (ok (string= "Alto" (stem-name s)))
    (ok (eq b (stem-buffer s)))
    (ok (= 2 (stem-track s)))
    (ok (null (stem-channel s)))))

(deftest condition-tree
  (ok (subtypep 'audio-format-error 'audio-error))
  (ok (subtypep 'audio-format-error 'parse-error))
  (ok (subtypep 'wav-unsupported-encoding 'audio-format-error))
  (ok (subtypep 'audio-encode-error 'audio-error))
  (ok (subtypep 'audio-mismatch-error 'audio-error))
  (ok (subtypep 'audio-limit-exceeded 'audio-error))
  (ok (subtypep 'audio-backend-unavailable 'audio-engine-error))
  (ok (subtypep 'audio-engine-error 'audio-error))
  (ok (subtypep 'samples-clipped 'audio-warning))
  (ok (subtypep 'wav-format-warning 'audio-warning))
  (ok (subtypep 'audio-warning 'warning))
  (ng (subtypep 'audio-warning 'error)))

(deftest conditions-report
  (flet ((text (c) (princ-to-string c)))
    (ok (search "no data chunk"
                (text (make-condition 'audio-format-error :offset 12 :reason "no data chunk"))))
    (ok (search "12" (text (make-condition 'audio-format-error :offset 12))))
    (ok (search "8 bits" (text (make-condition 'wav-unsupported-encoding :format-tag 1 :bits 8))))
    (ok (search "3 samples" (text (make-condition 'samples-clipped :count 3 :peak 1.5))))
    (ok (search "7" (text (make-condition 'audio-mismatch-error :index 7))))))

(deftest restart-functions-without-a-restart
  (ok (null (use-partial-data)))
  (ok (null (skip-input))))

(deftest data-tables
  (ok (equal '(:pcm16 :pcm24 :float32) +wav-encodings+))
  (ok (equal '(:clip :scale :none) +clip-policies+))
  (ok (= 16 +resample-zero-crossings+))
  (ok (= 0.95d0 +resample-cutoff+))
  (ok (= 3600 *max-render-seconds*)))
