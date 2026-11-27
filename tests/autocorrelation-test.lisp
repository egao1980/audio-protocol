;;;; AU3 lag functions: AUTOCORRELATION and YIN's d and d' against their
;;;; direct definitions, the exact zeros, the argument rules, and the cost.
;;;; Tolerances (autocorrelation.lisp): r within 1e-11 * E, d within
;;;; 1.1e-10 * E (E the frame's energy); d' is checked within 1e-8 of d' of
;;;; the direct d.

(in-package #:audio-protocol/tests)

(defun lag-call (fn frame &rest keys &key &allow-other-keys)
  "FN on FRAME with only the keywords whose values are not NIL."
  (apply fn frame (loop for (k v) on keys by #'cddr when v append (list k v))))

(deftest-parametrize autocorrelation-matches-the-sum
    ((n max-lag)
     :ids ("1" "2" "5" "64" "100" "1000" "2048" "2048-lag-300" "1000-lag-0" "777-lag-776")
     (1 nil) (2 nil) (5 nil) (64 nil) (100 nil) (1000 nil) (2048 nil) (2048 300) (1000 0)
     (777 776))
  (let* ((*seed* (+ 6000 n (or max-lag 0)))
         (x (random-reals n))
         (r (lag-call #'dsp:autocorrelation x :max-lag max-lag))
         (want (direct-autocorrelation x (or max-lag (1- n)))))
    (ok (typep r +double-vector+))
    (ok (= (length want) (length r)) "lags 0 .. MAX-LAG")
    (ok (within r want (* 1d-11 (energy x))) "within 1e-11 * E of the sum")))

(deftest autocorrelation-is-linear
  ;; A circular correlation would add x_3 x_0 at lag 1, and so on.
  (ok (within (dsp:autocorrelation #(1 2 3 4)) #(30 20 11 4) 1d-12))
  (ok (within (dsp:autocorrelation #(1 2 3 4) :max-lag 1) #(30 20) 1d-12))
  (ok (within (dsp:autocorrelation (make-array 3 :element-type 'single-float
                                                 :initial-contents '(0.5 -1.0 2.0)))
              #(5.25 -2.5 1) 1d-12))
  (ok (every #'zerop (dsp:autocorrelation (make-array 100 :initial-element 0d0)))
      "silence: exact zeros"))

(deftest-parametrize yin-difference-matches-the-sum
    ((n window max-lag)
     :ids ("2048" "2048-w512" "2048-w512-lag300" "1000" "777-w100" "64-w64" "2" "1")
     (2048 nil nil) (2048 512 nil) (2048 512 300) (1000 nil nil) (777 100 nil) (64 64 nil)
     (2 nil nil) (1 nil nil))
  (let* ((*seed* (+ 7000 n (or window 0) (or max-lag 0)))
         (x (random-reals n))
         (w (or window (max 1 (floor n 2))))
         (lag (or max-lag (- n w)))
         (d (lag-call #'dsp:yin-difference x :window window :max-lag max-lag)))
    (ok (typep d +double-vector+))
    (ok (= (1+ lag) (length d)) (format nil "W ~D: lags 0 .. ~D" w lag))
    (ok (within d (direct-yin-difference x w lag) (* 1.1d-10 (energy x))) "within 1.1e-10 * E")
    (ok (= 0 (aref d 0)) "d(0) = 0")
    (ok (every (lambda (v) (>= v 0)) d) "no value is negative")))

(deftest yin-difference-of-a-sine
  ;; Period 50 samples: d is 0 at 50, 100, ... (snapped from round-off), and
  ;; W A^2 (1 - cos(2 pi tau / 50)) in between.
  (let* ((x (map +double-vector+ (lambda (n) (* 0.8d0 (sin (/ (* 2 (dpi) n) 50))))
                 (loop for n below 2048 collect n)))
         (d (dsp:yin-difference x)))
    (ok (= 1025 (length d)) "W = 1024, lags 0 .. 1024 by default")
    (ok (loop for p from 50 to 1000 by 50 always (= 0 (aref d p))) "exact zeros at the period")
    (ok (= 25 (position (reduce #'max d :end 51) d :end 51)) "the maximum at half the period")
    (ok (within d (direct-yin-difference x 1024 1024) (* 1.1d-10 (energy x))))))

(deftest yin-difference-exact-zeros
  (let* ((pattern '(3 -1 4 1 -5 9 2 -6))
         (x (coerce (loop repeat 8 append pattern) 'vector))
         (d (dsp:yin-difference x)))
    (ok (= 33 (length d)))
    (ok (loop for tau from 0 to 32 always (if (zerop (mod tau 8)) (zerop (aref d tau)) (plusp (aref d tau))))
        "a frame repeating every 8 samples: 0 exactly at 0, 8, 16, 24, 32 and nowhere else"))
  (let ((d (dsp:yin-difference (make-array 512 :initial-element 0.3d0))))
    (ok (every #'zerop d) "a constant frame: zeros"))
  (let ((d (dsp:yin-difference (make-array 512 :initial-element 0))))
    (ok (every #'zerop d) "silence: zeros")))

(deftest cumulative-mean-normalize-by-hand
  (let ((d-prime (dsp:cumulative-mean-normalize #(0 2 4 0 6))))
    (ok (typep d-prime +double-vector+))
    (ok (within d-prime (vector 1 1 4/3 0 2) 1d-15)
        "1, 2/(2/1), 4/(6/2), 0/(6/3), 6/(12/4)"))
  (ok (within (dsp:cumulative-mean-normalize #(7 0 0 5)) #(1 1 1 3) 1d-15)
      "a zero sum gives 1; d(0) isn't used")
  (ok (within (dsp:cumulative-mean-normalize #(0)) #(1) 0) "lag 0 alone")
  (ok (within (dsp:cumulative-mean-normalize (vector 0 1/2 0.25 0.25d0)) #(1 1 2/3 3/4) 1d-15))
  (ok (signals (dsp:cumulative-mean-normalize #()) 'audio-error))
  (ok (signals (dsp:cumulative-mean-normalize '(0 1)) 'type-error)))

(deftest-parametrize yin-cmndf-matches-the-direct-d-prime
    ((n window) :ids ("2048" "1000-w300" "777") (2048 nil) (1000 300) (777 nil))
  (let* ((*seed* (+ 8000 n))
         (x (random-reals n))
         (w (or window (max 1 (floor n 2))))
         (d-prime (lag-call #'dsp:yin-cmndf x :window window)))
    (ok (typep d-prime +double-vector+))
    (ok (within d-prime (direct-cmnd (direct-yin-difference x w (- n w))) 1d-8))
    (ok (equalp d-prime (dsp:cumulative-mean-normalize (lag-call #'dsp:yin-difference x :window window)))
        "exactly CUMULATIVE-MEAN-NORMALIZE of YIN-DIFFERENCE")))

(deftest yin-cmndf-of-silence-and-of-a-period
  (ok (every (lambda (v) (= 1 v)) (dsp:yin-cmndf (make-array 256 :initial-element 0d0)))
      "silence: 1 everywhere, no dip")
  (ok (every (lambda (v) (= 1 v)) (dsp:yin-cmndf (make-array 256 :initial-element -0.5d0)))
      "a constant frame: 1 everywhere")
  (let* ((x (coerce (loop repeat 16 append '(3 -1 4 1 -5 9 2 -6)) 'vector))
         (d-prime (dsp:yin-cmndf x)))
    (ok (= 0 (aref d-prime 8)) "an exact period: d'(8) = 0")
    (ok (= 1 (aref d-prime 0)))
    (ok (= 8 (first-dip d-prime 2)) "YIN's threshold step finds it")))

(deftest lag-function-arguments
  (let ((x (random-reals 100)))
    (ok (signals (dsp:autocorrelation #()) 'audio-error) "an empty frame")
    (ok (signals (dsp:yin-difference #()) 'audio-error))
    (ok (signals (dsp:yin-cmndf #()) 'audio-error))
    (ok (signals (dsp:autocorrelation '(1 2)) 'type-error))
    (ok (signals (dsp:yin-difference (vector 1 #C(0 1))) 'type-error))
    (ok (signals (dsp:autocorrelation x :max-lag 100) 'audio-error) "MAX-LAG past N-1")
    (ok (signals (dsp:autocorrelation x :max-lag -1) 'audio-error))
    (ok (signals (dsp:autocorrelation x :max-lag 1.5) 'type-error))
    (ok (signals (dsp:yin-difference x :window 0) 'audio-error) "WINDOW 0")
    (ok (signals (dsp:yin-difference x :window 101) 'audio-error) "WINDOW past N")
    (ok (signals (dsp:yin-difference x :window 60 :max-lag 41) 'audio-error) "MAX-LAG past N - W")
    (ok (= 41 (length (dsp:yin-difference x :window 60 :max-lag 40))))
    (ok (= 41 (length (dsp:yin-difference x :window 60))) "MAX-LAG defaults to N - W")
    (ok (signals (dsp:yin-difference x :window 2.0) 'type-error))
    (ok (= 51 (length (dsp:yin-difference x))) "WINDOW defaults to N/2")
    (ok (= 100 (length (dsp:autocorrelation x))) "MAX-LAG defaults to N-1")))

(deftest lag-functions-cost-n-log-n
  ;; One frame 16 times longer must cost less than 3 times 32 short frames:
  ;; the direct sums would cost about 8 times as much, an FFT about 2/3.
  (let* ((*seed* 77)
         (big (random-reals 32768))
         (small (loop repeat 32 collect (random-reals 2048))))
    (dolist (fn (list #'dsp:autocorrelation #'dsp:yin-difference))
      (funcall fn big)
      (let ((t-big (best-time (lambda () (funcall fn big))))
            (t-small (best-time (lambda () (dolist (f small) (funcall fn f))))))
        (ok (< t-big (* 3 (max t-small 1)))
            (format nil "~A: 32768 samples ~D, 32 x 2048 samples ~D (internal time units)"
                    fn t-big t-small))))))
