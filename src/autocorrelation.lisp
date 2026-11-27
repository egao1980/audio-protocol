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

(defun autocorrelation (frame &key max-lag)
  "A fresh DOUBLE-VECTOR r(0) .. r(MAX-LAG): the linear autocorrelation of
FRAME, through the FFT. See autocorrelation.lisp."
  (declare (ignorable frame max-lag))
  (%au3-pending 'autocorrelation))

(defun yin-difference (frame &key window max-lag)
  "A fresh DOUBLE-VECTOR d(0) .. d(MAX-LAG): YIN's difference function of
FRAME over its first WINDOW samples, through the FFT. See autocorrelation.lisp."
  (declare (ignorable frame window max-lag))
  (%au3-pending 'yin-difference))

(defun cumulative-mean-normalize (difference)
  "A fresh DOUBLE-VECTOR d'(0) .. d'(T): YIN's cumulative mean normalized
difference of DIFFERENCE, d(0) .. d(T). See autocorrelation.lisp."
  (declare (ignorable difference))
  (%au3-pending 'cumulative-mean-normalize))

(defun yin-cmndf (frame &key window max-lag)
  "YIN's d' of FRAME: CUMULATIVE-MEAN-NORMALIZE of YIN-DIFFERENCE."
  (declare (ignorable frame window max-lag))
  (%au3-pending 'yin-cmndf))
