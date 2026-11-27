;;;; AU3 test support: the direct definitions the DSP functions must match
;;;; (plain sums, independent of the code under test), random signals, and
;;;; YIN's threshold step.

(in-package #:audio-protocol/tests)

(defparameter +double-vector+ '(simple-array double-float (*)))
(defparameter +complex-vector+ '(simple-array (complex double-float) (*)))

(defun random-reals (n &optional (scale 1d0))
  "A fresh vector of N pseudo-random DOUBLE-FLOATs in -SCALE..SCALE (RND)."
  (let ((v (make-array n :element-type 'double-float)))
    (dotimes (i n v)
      (setf (aref v i) (* scale (/ (- (rnd 2000001) 1000000) 1000000d0))))))

(defun random-complexes (n)
  "A fresh SIMPLE-VECTOR of N pseudo-random complex doubles, parts in -1..1."
  (let ((re (random-reals n))
        (im (random-reals n)))
    (map 'simple-vector #'complex re im)))

(defun abs-sum (x)
  "Sum of |x_n|: the scale of the FFT accuracy bounds."
  (loop for v across x sum (abs v)))

(defun energy (x)
  "Sum of x_n^2: the scale of the lag-function accuracy bounds."
  (loop for v across x sum (expt (float v 1d0) 2)))

(defun max-abs-diff (a b)
  "max |a_i - b_i|, or :LENGTHS when A and B differ in length."
  (if (/= (length a) (length b))
      :lengths
      (loop for x across a for y across b maximize (abs (- x y)))))

(defun within (a b tolerance)
  "True when A and B have one length and every |a_i - b_i| <= TOLERANCE."
  (let ((d (max-abs-diff a b)))
    (and (realp d) (<= d tolerance))))

(defun direct-dft (x &optional (sign -1))
  "sum_n x_n exp(SIGN 2 pi i k n / N) for each k, the angle reduced exactly
modulo N first."
  (let* ((n (length x))
         (out (make-array n)))
    (dotimes (k n out)
      (setf (aref out k)
            (loop for j below n
                  sum (* (aref x j) (cis (/ (* sign 2 (dpi) (mod (* k j) n)) n))))))))

(defun direct-idft (spectrum)
  (let ((n (length spectrum)))
    (map 'vector (lambda (v) (/ v n)) (direct-dft spectrum +1))))

(defun direct-autocorrelation (x max-lag)
  "r(tau) = sum_{n=0}^{N-1-tau} x_n x_{n+tau}, tau = 0 .. MAX-LAG."
  (let ((n (length x))
        (r (make-array (1+ max-lag) :element-type 'double-float)))
    (dotimes (tau (1+ max-lag) r)
      (setf (aref r tau)
            (loop for j below (- n tau)
                  sum (* (float (aref x j) 1d0) (aref x (+ j tau))))))))

(defun direct-yin-difference (x window max-lag)
  "d(tau) = sum_{j=0}^{W-1} (x_j - x_{j+tau})^2, tau = 0 .. MAX-LAG."
  (let ((d (make-array (1+ max-lag) :element-type 'double-float)))
    (dotimes (tau (1+ max-lag) d)
      (setf (aref d tau)
            (loop for j below window
                  sum (expt (- (float (aref x j) 1d0) (aref x (+ j tau))) 2))))))

(defun direct-cmnd (d)
  "YIN's d': 1 at lag 0, d(tau) over the mean of d(1) .. d(tau), 1 where that
mean is 0."
  (let ((out (make-array (length d) :element-type 'double-float))
        (sum 0d0))
    (dotimes (tau (length d) out)
      (if (zerop tau)
          (setf (aref out 0) 1d0)
          (progn
            (incf sum (aref d tau))
            (setf (aref out tau)
                  (if (zerop sum) 1d0 (/ (aref d tau) (/ sum tau)))))))))

(defun first-dip (d-prime from &optional (threshold 0.1d0))
  "YIN's absolute threshold (step 4): the first lag >= FROM with D-PRIME below
THRESHOLD, followed down to the bottom of its dip; NIL when there is none."
  (let ((start (position-if (lambda (v) (< v threshold)) d-prime :start from)))
    (when start
      (let ((tau start))
        (loop while (and (< (1+ tau) (length d-prime))
                         (< (aref d-prime (1+ tau)) (aref d-prime tau)))
              do (incf tau))
        tau))))

(defun dvector (&rest xs)
  "A fresh DOUBLE-VECTOR of XS."
  (map +double-vector+ (lambda (x) (float x 1d0)) xs))

(defun frames-as-lists (frames)
  (map 'list (lambda (f) (coerce f 'list)) frames))

(defun best-time (thunk &optional (runs 5))
  "The shortest of RUNS runs of THUNK, in internal time units."
  (loop repeat runs
        minimize (let ((start (get-internal-real-time)))
                   (funcall thunk)
                   (- (get-internal-real-time) start))))

(defun caught (thunk condition-type)
  "The condition of CONDITION-TYPE that THUNK signals (unwinding), or :NONE when it
returns. Other errors propagate, so a stub fails the test."
  (handler-case (progn (funcall thunk) :none)
    (error (c)
      (if (typep c condition-type) c (error c)))))
