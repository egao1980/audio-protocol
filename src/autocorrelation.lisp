;;;; AU3 lag functions: autocorrelation and the YIN difference functions,
;;;; computed through the FFT in O(N log N).
;;;;
;;;; A frame is a VECTOR of N REALs x_0 .. x_{N-1}, read as DOUBLE-FLOATs; a
;;;; list or another element type is a TYPE-ERROR, and N = 0 is an
;;;; AUDIO-ERROR. N is any length, not only a power of two: the functions pad
;;;; internally, and never signal FFT-SIZE-ERROR. Results are fresh
;;;; DOUBLE-VECTORs indexed by the lag tau, from 0. E = sum x_n^2 is the
;;;; frame's energy. WINDOW and MAX-LAG are integers (else TYPE-ERROR); a
;;;; value that doesn't fit the frame is an AUDIO-ERROR.
;;;;
;;;; (AUTOCORRELATION frame &key max-lag) -> r(0) .. r(MAX-LAG):
;;;;     r(tau) = sum_{n=0}^{N-1-tau} x_n x_{n+tau}
;;;;   This is the linear autocorrelation: nothing wraps around. MAX-LAG is
;;;;   0 .. N-1, by default N-1. Each value is within 1e-11 * E of the sum.
;;;;   Compute it through an FFT of the frame zero-padded to a power of two
;;;;   of at least N + MAX-LAG points, which keeps the circular correlation
;;;;   from wrapping.
;;;;
;;;; (YIN-DIFFERENCE frame &key window max-lag) -> d(0) .. d(MAX-LAG). This is
;;;;   the difference function of YIN (de Cheveigné and Kawahara 2002, step 2)
;;;;   over an integration window of W = WINDOW samples at the start of the
;;;;   frame:
;;;;     d(tau) = sum_{j=0}^{W-1} (x_j - x_{j+tau})^2
;;;;   WINDOW is 1 .. N, by default max(1, floor(N/2)). MAX-LAG is 0 .. N - W,
;;;;   by default N - W, so every x_{j+tau} lies in the frame. (A frame of 2048
;;;;   samples at 16 kHz: W = 1024, lags up to 1024, so periods down to
;;;;   15.6 Hz.) Expanded,
;;;;     d(tau) = e(0) + e(tau) - 2 c(tau), where
;;;;     e(tau) = sum_{j=tau}^{tau+W-1} x_j^2,   c(tau) = sum_{j=0}^{W-1} x_j x_{j+tau}.
;;;;   Compute c through the FFT (the cross-correlation of the first W samples
;;;;   with the frame; a power of two of at least N points is enough, since
;;;;   j + tau < N), and e with running sums. The cost is then O(N log N),
;;;;   not the O(N W) of the sum.
;;;;   Compute each value within 1e-11 * E of the sum. Then round-off is
;;;;   cleaned up, so that the zeros YIN looks for are exact zeros: every
;;;;   value below 1e-10 * E (100 dB under the frame's energy) becomes 0d0.
;;;;   So each result is within 1.1e-10 * E of the sum, no value is negative,
;;;;   d(0) is 0d0, and every d(tau) that is 0 in exact arithmetic is 0d0:
;;;;   at every lag for silence and for a constant frame, and at the period
;;;;   of a frame that repeats exactly.
;;;;
;;;; (CUMULATIVE-MEAN-NORMALIZE d) -> d'(0) .. d'(T) for d = d(0) .. d(T), a
;;;;   VECTOR of T + 1 >= 1 REALs (else TYPE-ERROR, or AUDIO-ERROR when
;;;;   empty). This is YIN's step 3, the cumulative mean normalized difference:
;;;;     d'(0) = 1,   d'(tau) = d(tau) / ((1/tau) sum_{j=1}^{tau} d(j))   (tau >= 1)
;;;;   in DOUBLE-FLOAT, the sum running in order of tau, so d(0) isn't used.
;;;;   When that sum is 0 (zeros so far, as in silence), d'(tau) = 1: no dip,
;;;;   so no evidence of a period. (Adding a tiny constant to the denominator
;;;;   instead would make silence look perfectly periodic.) pYIN (Mauch and
;;;;   Dixon 2014) thresholds this same d'.
;;;;
;;;; (YIN-CMNDF frame &key window max-lag) ->
;;;;   (cumulative-mean-normalize (yin-difference frame :window window
;;;;                                              :max-lag max-lag)),
;;;;   EQUALP to that composition.
;;;;
;;;; Cost, as tested. AUTOCORRELATION and YIN-DIFFERENCE of one frame of
;;;; 32768 samples (default arguments) take less than three times as long as
;;;; 32 frames of 2048 samples (the best of several runs of each). The direct
;;;; sums would take about eight times as long, since a frame 16 times longer
;;;; is 256 times their work; through an FFT it takes about two thirds as
;;;; long.

(in-package #:audio-protocol/dsp)

;;; Internal helpers. They never reach the size-checked public transforms, so
;;; the lag functions never signal FFT-SIZE-ERROR.

(defun %check-not-empty (n what)
  "AUDIO-ERROR when N, the length of WHAT, is 0."
  (when (zerop n)
    (error 'audio-error :reason (format nil "an empty ~A" what))))

(defun %correlate (x w points count)
  "c(tau) = sum_{j<W} x_j x_{j+tau} for tau below COUNT, as a fresh
DOUBLE-VECTOR, x_k being 0 past the end of the DOUBLE-VECTOR X. It is computed
as the circular cross-correlation of A, the first W samples of X, with X, both
zero-padded to POINTS (a power of two). No index wraps when
(W - 1) + (COUNT - 1) < POINTS, and the callers choose POINTS so. With
Y = conj(A) X, c = IDFT(Y) = conj(DFT(conj Y)) / POINTS, and c is real, so one
forward table serves every transform. W = N is the autocorrelation, with one
transform fewer."
  (declare (type double-vector x))
  (multiple-value-bind (cs sn) (%twiddles points -1)
    (let ((ar (%padded x points w))
          (ai (%dvec points)))
      (declare (type double-vector ar ai))
      (%fft-split! ar ai cs sn)
      (if (= w (length x))
          (dotimes (k points)                 ; conj(Y) = |A|^2
            (setf (aref ar k) (+ (* (aref ar k) (aref ar k)) (* (aref ai k) (aref ai k)))
                  (aref ai k) 0d0))
          (let ((xr (%padded x points))
                (xi (%dvec points)))
            (declare (type double-vector xr xi))
            (%fft-split! xr xi cs sn)
            (dotimes (k points)               ; conj(Y) = A conj(X)
              (let ((p (aref ar k))
                    (q (aref ai k))
                    (u (aref xr k))
                    (v (aref xi k)))
                (setf (aref ar k) (+ (* p u) (* q v))
                      (aref ai k) (- (* q u) (* p v)))))))
      (%fft-split! ar ai cs sn)
      (let ((c (%dvec count))
            (scale (/ 1d0 points)))           ; exact: POINTS is a power of two
        (dotimes (tau count c)
          (setf (aref c tau) (* scale (aref ar tau))))))))

(defun %energy-prefix (x)
  "Q_0 .. Q_N with Q_k = sum_{j<k} x_j^2, as a fresh DOUBLE-VECTOR of N + 1
compensated (Neumaier) running sums. Each is within a few ulps of its exact
value for any N, so a difference Q_b - Q_a is within a few ulps of E."
  (declare (type double-vector x))
  (let* ((n (length x))
         (q (%dvec (1+ n)))
         (s 0d0)
         (compensation 0d0))
    (declare (type double-float s compensation))
    (dotimes (i n q)
      (let* ((v (* (aref x i) (aref x i)))
             (sum (+ s v)))
        (declare (type double-float v sum))
        (if (>= (abs s) (abs v))
            (incf compensation (+ (- s sum) v))
            (incf compensation (+ (- v sum) s)))
        (setf s sum
              (aref q (1+ i)) (+ s compensation))))))

;;; The lag functions ---------------------------------------------------------------

(defun autocorrelation (frame &key max-lag)
  "A fresh DOUBLE-VECTOR r(0) .. r(MAX-LAG): the linear autocorrelation of
FRAME, through the FFT. See autocorrelation.lisp."
  (let ((x (%real-input frame)))
    (check-type max-lag (or null integer))
    (let ((n (length x)))
      (%check-not-empty n "frame")
      (let ((max-lag (or max-lag (1- n))))
        (unless (<= 0 max-lag (1- n))
          (error 'audio-error
                 :reason (format nil "max-lag ~D is not in 0 .. ~D" max-lag (1- n))))
        ;; N + MAX-LAG points: n + tau <= N - 1 + MAX-LAG never wraps.
        (%correlate x n (next-power-of-two (+ n max-lag)) (1+ max-lag))))))

(defun yin-difference (frame &key window max-lag)
  "A fresh DOUBLE-VECTOR d(0) .. d(MAX-LAG): YIN's difference function of
FRAME over its first WINDOW samples, through the FFT. See autocorrelation.lisp."
  (let ((x (%real-input frame)))
    (declare (type double-vector x))
    (check-type window (or null integer))
    (check-type max-lag (or null integer))
    (let ((n (length x)))
      (%check-not-empty n "frame")
      (let ((w (or window (max 1 (floor n 2)))))
        (unless (<= 1 w n)
          (error 'audio-error
                 :reason (format nil "window ~D is not in 1 .. ~D" w n)))
        (let ((max-lag (or max-lag (- n w))))
          (unless (<= 0 max-lag (- n w))
            (error 'audio-error
                   :reason (format nil "max-lag ~D is not in 0 .. ~D (N - W)" max-lag (- n w))))
          ;; d(tau) = e(0) + e(tau) - 2 c(tau). j + tau <= N - 1, so N points
          ;; padded to a power of two are enough for c.
          (let* ((c (%correlate x w (next-power-of-two n) (1+ max-lag)))
                 (q (%energy-prefix x))
                 (tiny (* 1d-10 (aref q n)))      ; 1e-10 E
                 (e0 (aref q w))
                 (d (%dvec (1+ max-lag))))        ; d(0) = 0d0 exactly
            (declare (type double-vector c q d) (type double-float tiny e0))
            (loop for tau from 1 to max-lag
                  do (let ((v (- (+ e0 (- (aref q (+ tau w)) (aref q tau)))
                                 (* 2 (aref c tau)))))
                       ;; Round-off below 1e-10 E, and any value <= 0, is 0d0.
                       (setf (aref d tau) (if (or (<= v 0d0) (< v tiny)) 0d0 v))))
            d))))))

(defun cumulative-mean-normalize (difference)
  "A fresh DOUBLE-VECTOR d'(0) .. d'(T): YIN's cumulative mean normalized
difference of DIFFERENCE, d(0) .. d(T). See autocorrelation.lisp."
  (let* ((d (%real-input difference))
         (n (length d)))
    (declare (type double-vector d))
    (%check-not-empty n "difference function")
    (let ((out (%dvec n))
          (sum 0d0))
      (declare (type double-float sum))
      (setf (aref out 0) 1d0)
      (loop for tau from 1 below n
            do (incf sum (aref d tau))
               (setf (aref out tau)
                     (if (zerop sum) 1d0 (/ (aref d tau) (/ sum tau)))))
      out)))

(defun yin-cmndf (frame &key window max-lag)
  "YIN's d' of FRAME: CUMULATIVE-MEAN-NORMALIZE of YIN-DIFFERENCE."
  (cumulative-mean-normalize (yin-difference frame :window window :max-lag max-lag)))
