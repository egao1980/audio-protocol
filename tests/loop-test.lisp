;;;; AU0 LOOP-REGION: a passage repeated, with gaps and fades.

(in-package #:audio-protocol/tests)

(defun ramp-buffer (frames &key (channels 1) (rate 8000))
  "Frame i of every channel holds (i + 1) / 64, so frames are easy to tell apart."
  (make-pcm-buffer :sample-rate rate :channels channels
                   :samples (loop for i below frames
                                  nconc (make-list channels :initial-element (/ (1+ i) 64)))))

(deftest loop-copies-the-region
  (let* ((b (ramp-buffer 10))
         (y (loop-region b 2 5)))
    (ok (= 6 (pcm-buffer-frames y)) "count 2 by default")
    (ok (equalp (mapcar #'single '(3/64 4/64 5/64 3/64 4/64 5/64)) (samples y)))
    (ok (= 8000 (pcm-buffer-sample-rate y)))))

(deftest-parametrize loop-layout
    ((start end count gap)
     :rows '((0 10 1 0) (0 1 3 0) (4 8 3 2) (9 10 2 5) (0 10 4 1)))
  (let* ((b (ramp-buffer 10 :channels 2))
         (y (loop-region b start end :count count :gap gap))
         (n (- end start))
         (want (loop for j below count
                     nconc (loop for k below n
                                 nconc (make-list 2 :initial-element
                                                  (single (/ (+ start k 1) 64))))
                     when (< j (1- count))
                       nconc (make-list (* 2 gap) :initial-element 0.0))))
    (ok (= (+ (* count n) (* (1- count) gap)) (pcm-buffer-frames y)))
    (ok (= 2 (pcm-buffer-channels y)))
    (ok (equalp want (samples y)))))

(deftest loop-fades
  (let* ((b (make-pcm-buffer :sample-rate 8000 :channels 2
                             :samples (loop repeat 20 nconc (list 0.5 -0.25))))
         (n 8) (fade 3)
         (y (loop-region b 4 12 :count 2 :fade fade :gap 1))
         (gain (lambda (k) (min 1 (/ k fade) (/ (- n 1 k) fade))))
         (play (loop for k below n
                     nconc (list (single (* (float (funcall gain k) 1d0) 0.5d0))
                                 (single (* (float (funcall gain k) 1d0) -0.25d0))))))
    (ok (= 17 (pcm-buffer-frames y)))
    (ok (equalp (append play (list 0.0 0.0) play) (samples y)))
    (ok (zerop (sample-ref y 0 0)) "first frame of a play is 0")
    (ok (zerop (sample-ref y 7 1)) "last frame of a play is 0")
    (ok (= 0.5 (sample-ref y 3 0)) "full gain after the fade")))

(deftest the-longest-fade
  (let ((y (loop-region (ramp-buffer 4) 0 4 :count 1 :fade 2)))
    (ok (equalp (mapcar #'single (list 0 (* 1/2 2/64) (* 1/2 3/64) 0)) (samples y)))))

(deftest-parametrize loop-region-bounds
    ((start end keys)
     :rows '((5 5 ()) (6 5 ()) (0 11 ()) (-1 3 ()) (0 4 (:fade 3)) (0 4 (:count 0))
             (0 4 (:gap -1))))
  (ok (signals (apply #'loop-region (ramp-buffer 10) start end keys) 'audio-error)))

(deftest loop-region-argument-types
  (ok (signals (loop-region (ramp-buffer 10) 0.5 3) 'type-error))
  (ok (signals (loop-region (ramp-buffer 10) 0 3 :count 1.5) 'type-error))
  (ok (signals (loop-region (ramp-buffer 10) 0 3 :fade :short) 'type-error))
  (ok (signals (loop-region '(1 2) 0 1) 'type-error))
  (ok (signals (loop-region (ramp-buffer 10) 5 5) 'audio-error))
  (ok (signals (loop-region (ramp-buffer 10) 0 11) 'audio-error))
  (ok (signals (loop-region (ramp-buffer 10) 0 4 :fade 3) 'audio-error)))

(deftest loop-region-leaves-its-buffer-alone
  (let ((b (ramp-buffer 6)) (copy (ramp-buffer 6)))
    (loop-region b 1 5 :fade 2)
    (ok (equalp copy b))))
