;;;; AU0 resampling: a windowed-sinc interpolator at exact rational positions.
;;;;
;;;; (RESAMPLE buffer sample-rate) -> a fresh PCM-BUFFER at SAMPLE-RATE (an
;;;; integer 1..#xFFFFFFFF, else TYPE-ERROR), BUFFER's channels.
;;;;
;;;; Same rate: a copy, EQUALP to BUFFER.
;;;;
;;;; Otherwise, with R = SAMPLE-RATE / (pcm-buffer-sample-rate buffer), an exact
;;;; rational:
;;;;   frames  F' = (seconds-to-frame (pcm-buffer-duration buffer) SAMPLE-RATE),
;;;;           i.e. floor(F * R + 1/2) for F input frames.
;;;;   time    output frame j sits at input position t_j = j / R (exact).
;;;;   value   for each channel, with x_n the input sample of frame n (0 outside
;;;;           0 .. F-1),
;;;;               y_j = sum over integers n with |t_j - n| < L of x_n * h(t_j - n)
;;;;           rounded to a SINGLE-FLOAT, where
;;;;               h(t)  = c * sinc(c * t) * w(t / L)
;;;;               c     = +RESAMPLE-CUTOFF+ * min(1, R)    (0.95 of the lower Nyquist)
;;;;               L     = +RESAMPLE-ZERO-CROSSINGS+ / c     (16 zero crossings a side)
;;;;               sinc(u) = sin(pi u) / (pi u), sinc(0) = 1
;;;;               w(u)  = 0.42 + 0.5 cos(pi u) + 0.08 cos(2 pi u)   (Blackman)
;;;;           computed in DOUBLE-FLOAT. An implementation may tabulate h (one
;;;;           phase per distinct fraction of t_j, as a polyphase filter does) as
;;;;           long as every y_j stays within 1e-5 of the formula for inputs in
;;;;           -1..1.
;;;;
;;;; Quality. Passband flat to 0.95 of the lower Nyquist frequency (within
;;;; 0.1 dB well inside it); the Blackman window's stopband is about -74 dB, so
;;;; content above the new Nyquist frequency (when downsampling) is attenuated
;;;; by more than 60 dB once past the transition band, whose width is about
;;;; 5.5 / (2L) of the input rate (44100 -> 16000: from about 6.3 to 8.9 kHz).
;;;; That is the quality a transcription front end at 16 kHz needs; it is not a
;;;; mastering-grade converter. The edges are zero-padded, so the first and
;;;; last L input frames ramp. Cost: about 2L multiply-adds per output sample
;;;; (93 for 44100 -> 16000).

(in-package #:audio-protocol)

(defconstant +resample-zero-crossings+ 16
  "Zero crossings of the resampling kernel on each side of its centre.")

(defconstant +resample-cutoff+ 0.95d0
  "Cutoff of the resampling kernel, as a fraction of the lower Nyquist frequency.")

(defun resample (buffer sample-rate)
  "BUFFER converted to SAMPLE-RATE with a Blackman-windowed sinc (see the file
header). Same rate: a copy."
  (declare (ignorable buffer sample-rate))
  (%au0-pending 'resample))
