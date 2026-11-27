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

(defun %level-samples (x)
  "Every sample of X, a PCM-BUFFER of any channel count or a vector of reals,
as a fresh DOUBLE-VECTOR."
  (%real-input (if (pcm-buffer-p x) (pcm-buffer-samples x) x)))

(defun %peak (v)
  "max |v_m| of the DOUBLE-VECTOR V, from 0d0."
  (declare (type double-vector v))
  (let ((p 0d0))
    (declare (type double-float p))
    (dotimes (i (length v) p)
      (setf p (max p (abs (aref v i)))))))

(defun rms (x)
  "The root mean square of every sample of X (a PCM-BUFFER or a vector of
reals), a DOUBLE-FLOAT; 0d0 for no samples."
  (check-type x (or vector pcm-buffer))
  (let* ((v (%level-samples x))
         (m (length v))
         (sum 0d0))
    (declare (type double-vector v) (type double-float sum))
    (dotimes (i m)
      (incf sum (* (aref v i) (aref v i))))
    (if (zerop m) 0d0 (sqrt (/ sum m)))))

(defun peak (x)
  "The largest |sample| of X (a PCM-BUFFER or a vector of reals), a
DOUBLE-FLOAT; 0d0 for no samples."
  (check-type x (or vector pcm-buffer))
  (%peak (%level-samples x)))

(defun frame-rms (signal size hop &key (pad :center))
  "A fresh DOUBLE-VECTOR: the RMS of each frame of SIGNAL (see FRAME-SIGNAL)."
  (check-type signal (or vector pcm-buffer))
  (check-type size (integer 1))
  (check-type hop (integer 1))
  (%check-member pad +pad-policies+)
  (let* ((x (%signal-samples signal))
         (length (length x))
         (count (%frame-count length size hop pad))
         (out (%dvec count)))
    (declare (type double-vector x))
    ;; Each frame is summed on its own, over its samples inside the signal (the
    ;; padding adds zeros), so a silent frame is exactly 0 whatever came before.
    (dotimes (i count out)
      (let ((start (%frame-start i size hop pad))
            (sum 0d0))
        (declare (type double-float sum))
        (loop for j from (max 0 (- start)) below (min size (- length start))
              do (let ((v (aref x (+ start j))))
                   (incf sum (* v v))))
        (setf (aref out i) (sqrt (/ sum size)))))))

(defun normalize (x &key (peak-db 0))
  "X scaled so that its peak is PEAK-DB dBFS: a fresh PCM-BUFFER for a buffer,
a fresh DOUBLE-VECTOR for a vector. Silence is copied unscaled."
  (check-type x (or vector pcm-buffer))
  (check-type peak-db real)
  (let* ((v (%level-samples x))
         (p (%peak v)))
    (declare (type double-vector v))
    (unless (zerop p)
      (let ((g (/ (db-to-gain peak-db) p)))
        (dotimes (i (length v))
          (setf (aref v i) (* g (aref v i))))))
    (if (pcm-buffer-p x)
        ;; MAKE-PCM-BUFFER copies, rounding each sample once to a SINGLE-FLOAT.
        (make-pcm-buffer :sample-rate (pcm-buffer-sample-rate x)
                         :channels (pcm-buffer-channels x)
                         :samples v)
        v)))

(defun downmix (buffer)
  "A fresh mono PCM-BUFFER: the mean of BUFFER's channels, frame by frame."
  (check-type buffer pcm-buffer)
  (let* ((channels (pcm-buffer-channels buffer))
         (frames (pcm-buffer-frames buffer))
         (x (pcm-buffer-samples buffer))
         (out (%dvec frames)))
    (dotimes (n frames)
      (let ((sum 0d0)
            (base (* n channels)))
        (declare (type double-float sum))
        (dotimes (k channels)
          (incf sum (float (aref x (+ base k)) 1d0)))
        (setf (aref out n) (/ sum channels))))
    (make-pcm-buffer :sample-rate (pcm-buffer-sample-rate buffer)
                     :channels 1
                     :samples out)))
