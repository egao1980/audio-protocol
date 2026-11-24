;;;; AU0 RESAMPLE: lengths, the kernel formula, and the quality it promises.

(in-package #:audio-protocol/tests)

(defun kernel-value (buffer channel rate j)
  "Output sample J of channel CHANNEL resampling BUFFER to RATE, straight from
the formula in resample.lisp (independent of the implementation)."
  (let* ((in-rate (pcm-buffer-sample-rate buffer))
         (r (/ rate in-rate))
         (c (* 0.95d0 (min 1 r)))
         (l (/ 16 c))
         (tj (/ j r))
         (x (pcm-channel buffer channel))
         (sum 0d0))
    (loop for n from (max 0 (ceiling (- tj l))) to (min (1- (length x)) (floor (+ tj l)))
          for d = (float (- tj n) 1d0)
          when (< (abs d) l)
            do (let* ((u (* c d))
                      (sinc (if (zerop u) 1d0 (/ (sin (* (dpi) u)) (* (dpi) u))))
                      (v (/ d l))
                      (w (+ 0.42d0 (* 0.5d0 (cos (* (dpi) v))) (* 0.08d0 (cos (* 2 (dpi) v))))))
                 (incf sum (* (aref x n) c sinc w))))
    sum))

(defun tone (rate frequency seconds &key (amplitude 0.5d0) (channels 1))
  (let ((v (sine-vector (seconds-to-frame seconds rate) rate frequency :amplitude amplitude)))
    (pcm-from-channels (make-list channels :initial-element v) :sample-rate rate)))

(deftest same-rate-is-a-copy
  (let* ((b (tone 8000 440 1/10))
         (y (resample b 8000)))
    (ok (equalp b y))
    (ok (not (eq (pcm-buffer-samples b) (pcm-buffer-samples y))))))

(deftest-parametrize resampled-lengths
    ((frames from to want)
     :rows '((44100 44100 16000 16000) (1000 44100 16000 363) (1001 44100 16000 363)
             (11 8000 16000 22) (3 48000 16000 1) (2 48000 16000 1) (1 48000 16000 0)
             (0 8000 44100 0) (160 16000 44100 441) (7 22050 44100 14)))
  (let ((y (resample (make-pcm-buffer :sample-rate from :frames frames :channels 2) to)))
    (ok (= want (pcm-buffer-frames y)))
    (ok (= to (pcm-buffer-sample-rate y)))
    (ok (= 2 (pcm-buffer-channels y)))))

(deftest-parametrize follows-the-formula
    ((from to) :rows '((44100 16000) (16000 44100) (8000 11025) (48000 44100) (22050 8000)))
  (let* ((*seed* (+ from to))
         (b (make-pcm-buffer :sample-rate from :channels 2
                             :samples (loop repeat 600 collect (/ (- (rnd 20001) 10000) 10000))))
         (y (resample b to))
         (bad '()))
    (dolist (j (list 0 1 2 (floor (pcm-buffer-frames y) 2) (- (pcm-buffer-frames y) 2)
                     (- (pcm-buffer-frames y) 1)))
      (dotimes (ch 2)
        (unless (approx= (kernel-value b ch to j) (sample-ref y j ch) 1d-5)
          (push (list j ch) bad))))
    (ok (null bad) (report bad))))

(defun interior-rms (buffer &optional (margin 1/20))
  "RMS of channel 0 away from the edges (MARGIN seconds at each end)."
  (let ((v (pcm-channel buffer 0))
        (skip (seconds-to-frame margin (pcm-buffer-sample-rate buffer))))
    (rms v skip (- (length v) skip))))

(deftest passband-keeps-amplitude-and-frequency
  (dolist (case '((44100 16000 1000) (44100 16000 4000) (16000 48000 1000) (48000 44100 3000)
                  (8000 22050 2500)))
    (destructuring-bind (from to frequency) case
      (let* ((y (resample (tone from frequency 1/2) to))
             (v (pcm-channel y 0))
             (skip (seconds-to-frame 1/20 to)))
        (ok (approx= 0 (* 20 (log (/ (interior-rms y) (/ 0.5d0 (sqrt 2d0))) 10)) 0.1d0)
            (format nil "~D -> ~D at ~D Hz: level within 0.1 dB" from to frequency))
        (ok (approx= frequency (zero-crossing-frequency v skip (- (length v) skip) to)
                     (* 0.005 frequency))
            (format nil "~D -> ~D at ~D Hz: frequency kept" from to frequency))))))

(deftest stopband-attenuates-aliases
  ;; Downsampling to 16 kHz: tones above 8.9 kHz must not fold back.
  (dolist (frequency '(10000 12000 15000 20000))
    (let ((y (resample (tone 44100 frequency 1/2) 16000)))
      (ok (< (/ (interior-rms y) (/ 0.5d0 (sqrt 2d0))) 1d-3)
          (format nil "~D Hz down by more than 60 dB" frequency)))))

(deftest upsampled-tone-is-the-tone
  (let* ((y (resample (tone 16000 1000 1/4) 48000))
         (v (pcm-channel y 0))
         (bad (loop for i from 2400 below (- (length v) 2400)
                    unless (approx= (* 0.5d0 (sin (/ (* 2 (dpi) 1000 i) 48000))) (aref v i) 2d-3)
                      collect i)))
    (ok (null bad) (report bad))))

(deftest dc-stays-dc
  (let* ((b (make-pcm-buffer :sample-rate 44100 :samples (make-list 8820 :initial-element 0.5)))
         (y (resample b 16000))
         (v (pcm-channel y 0)))
    (ok (every (lambda (x) (approx= 0.5d0 x 1d-3)) (subseq v 200 (- (length v) 200))))))

(deftest channels-are-independent
  (let* ((left (sine-vector 4410 44100 440))
         (b (pcm-from-channels (list left (make-array 4410 :element-type 'single-float
                                                           :initial-element 0.0))
                               :sample-rate 44100))
         (y (resample b 16000)))
    (ok (every #'zerop (pcm-channel y 1)) "silence stays exactly silent")
    (ok (equalp (pcm-channel (resample (pcm-from-channels (list left) :sample-rate 44100) 16000) 0)
                (pcm-channel y 0)))))

(deftest resample-argument-errors
  (ok (signals (resample (tone 8000 440 1/100) 0) 'type-error))
  (ok (signals (resample (tone 8000 440 1/100) 8000.0) 'type-error))
  (ok (signals (resample '(1 2) 8000) 'type-error)))
