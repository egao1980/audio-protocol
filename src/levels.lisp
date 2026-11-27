;;;; AU3 levels: RMS, PEAK, FRAME-RMS, NORMALIZE, and DOWNMIX.
;;;;
;;;; RMS, PEAK and NORMALIZE take a PCM-BUFFER of any channel count or a VECTOR
;;;; of REALs (else TYPE-ERROR). For a buffer, every sample of every channel
;;;; counts. The M samples are x_0 .. x_{M-1}, and arithmetic is DOUBLE-FLOAT.
;;;;
;;;; (RMS x) -> sqrt((1/M) sum x_m^2), a DOUBLE-FLOAT; 0d0 when M = 0.
;;;; (PEAK x) -> max |x_m|, a DOUBLE-FLOAT; 0d0 when M = 0.
;;;;
;;;; (FRAME-RMS signal size hop &key (pad :center)) -> a fresh DOUBLE-VECTOR
;;;;   with the RMS of each frame of (frame-signal signal size hop :pad pad),
;;;;   over SIZE samples, padding zeros included. SIGNAL, SIZE, HOP and PAD are
;;;;   as in frames.lisp (one channel). Each value is non-negative and within
;;;;   1e-9 * (peak signal) of (rms frame). A running sum of squares over the
;;;;   whole signal loses that precision on a quiet frame after a loud one, and
;;;;   can go negative, so sum each frame on its own (or keep the error that
;;;;   small another way). A silent frame then has RMS 0.
;;;;
;;;; (NORMALIZE x &key (peak-db 0)) -> X scaled so that its peak is
;;;;   (db-to-gain PEAK-DB): every sample times g = (db-to-gain peak-db) /
;;;;   (peak x), in DOUBLE-FLOAT. PEAK-DB is a real, else TYPE-ERROR.
;;;;   A PCM-BUFFER gives a fresh PCM-BUFFER of its sample rate, channels and
;;;;   frames, each sample rounded once to a SINGLE-FLOAT. Nothing is clipped:
;;;;   a PEAK-DB above 0 leaves samples above 1, as MIX's :NONE does, for
;;;;   WRITE-WAV to warn about. Any other VECTOR gives a fresh DOUBLE-VECTOR.
;;;;   When the peak is 0 (silence, or no samples) nothing is scaled: the
;;;;   result is a fresh copy (a buffer EQUALP to X, or a DOUBLE-VECTOR of X's
;;;;   values).
;;;;
;;;; (DOWNMIX buffer) -> a fresh one-channel PCM-BUFFER with BUFFER's sample
;;;;   rate and frames. Frame n is the mean of BUFFER's channels at frame n:
;;;;   their sum in channel order, in DOUBLE-FLOAT, divided by the channel
;;;;   count, rounded once to a SINGLE-FLOAT. A mono buffer gives a copy
;;;;   (EQUALP). A stereo buffer gives exactly
;;;;   (mix (list buffer) :channels 1 :clip :none), whose matrix is (1/2 1/2).
;;;;   BUFFER is a PCM-BUFFER, else TYPE-ERROR.

(in-package #:audio-protocol/dsp)

(defun rms (x)
  "The root mean square of every sample of X (a PCM-BUFFER or a vector of
reals), a DOUBLE-FLOAT; 0d0 for no samples."
  (declare (ignorable x))
  (%au3-pending 'rms))

(defun peak (x)
  "The largest |sample| of X (a PCM-BUFFER or a vector of reals), a
DOUBLE-FLOAT; 0d0 for no samples."
  (declare (ignorable x))
  (%au3-pending 'peak))

(defun frame-rms (signal size hop &key (pad :center))
  "A fresh DOUBLE-VECTOR: the RMS of each frame of SIGNAL (see FRAME-SIGNAL)."
  (declare (ignorable signal size hop pad))
  (%au3-pending 'frame-rms))

(defun normalize (x &key (peak-db 0))
  "X scaled so that its peak is PEAK-DB dBFS: a fresh PCM-BUFFER for a buffer,
a fresh DOUBLE-VECTOR for a vector. Silence is copied unscaled."
  (declare (ignorable x peak-db))
  (%au3-pending 'normalize))

(defun downmix (buffer)
  "A fresh mono PCM-BUFFER: the mean of BUFFER's channels, frame by frame."
  (declare (ignorable buffer))
  (%au3-pending 'downmix))
