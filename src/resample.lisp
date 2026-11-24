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

(defconstant +resample-table-limit+ (expt 2 20)
  "Most kernel values RESAMPLE keeps in its phase table (one row per distinct
fraction of t_j). Past it, every output frame computes its own row with the
same function, so the two paths give the same samples.")

(defun %resample-kernel (d c l)
  "h(D) = C sinc(C D) w(D / L), the file header's kernel, in DOUBLE-FLOAT."
  (declare (type double-float d c l))
  (let* ((pi-d (coerce pi 'double-float))
         (u (* c d))
         (sinc (if (zerop u) 1d0 (/ (sin (* pi-d u)) (* pi-d u))))
         (v (/ d l))
         (w (+ 0.42d0 (* 0.5d0 (cos (* pi-d v))) (* 0.08d0 (cos (* 2 pi-d v))))))
    (* c sinc w)))

(defun %resample-taps (frac c l)
  "(m0 . taps): TAPS holds h(FRAC - m) for the integers m from
ceiling(FRAC - L) to floor(FRAC + L), m0 the first; FRAC is an exact rational
in [0, 1), and a tap with |FRAC - m| not below L is 0."
  (let* ((m0 (ceiling (- frac l)))
         (m1 (floor (+ frac l)))
         (taps (make-array (max 0 (1+ (- m1 m0))) :element-type 'double-float
                                                  :initial-element 0d0)))
    (loop for m from m0 to m1
          for i from 0
          do (let ((d (float (- frac m) 1d0)))
               (when (< (abs d) l)
                 (setf (aref taps i) (%resample-kernel d c l)))))
    (cons m0 taps)))

(defun resample (buffer sample-rate)
  "BUFFER converted to SAMPLE-RATE with a Blackman-windowed sinc (see the file
header). Same rate: a copy."
  (check-type buffer pcm-buffer)
  (check-type sample-rate (integer 1 #xFFFFFFFF))
  (let ((in-rate (pcm-buffer-sample-rate buffer))
        (channels (pcm-buffer-channels buffer))
        (frames (pcm-buffer-frames buffer))
        (x (pcm-buffer-samples buffer)))
    (if (= sample-rate in-rate)
        (%make-pcm-buffer in-rate channels frames (copy-seq x))
        (let* ((r (/ sample-rate in-rate))
               ;; t_j = j / R = j q / p: (floor (* j q) p) is the integer part
               ;; and the phase, and t_j - n = phase/p - m exactly.
               (p (numerator r))
               (q (denominator r))
               (out-frames (seconds-to-frame (pcm-buffer-duration buffer) sample-rate))
               (c (* +resample-cutoff+ (min 1 r)))
               (l (/ +resample-zero-crossings+ c))
               (table (when (<= (* p (+ 2 (* 2 (ceiling l)))) +resample-table-limit+)
                        (make-array p :initial-element nil)))
               (y (%sample-vector (* out-frames channels))))
          (declare (type sample-vector x y))
          (dotimes (j out-frames)
            (multiple-value-bind (base phase) (floor (* j q) p)
              (let ((row (or (and table (svref table phase))
                             (let ((row (%resample-taps (/ phase p) c l)))
                               (when table (setf (svref table phase) row))
                               row))))
                (destructuring-bind (m0 . taps) row
                  (declare (type (simple-array double-float (*)) taps))
                  ;; Taps i with input frame n = BASE + M0 + i in 0..FRAMES-1.
                  (let* ((first-n (+ base m0))
                         (lo (max 0 (- first-n)))
                         (hi (min (length taps) (- frames first-n))))
                    (dotimes (ch channels)
                      (let ((sum 0d0))
                        (declare (type double-float sum))
                        (loop for i from lo below hi
                              do (incf sum (* (aref x (+ (* (+ first-n i) channels) ch))
                                              (aref taps i))))
                        (setf (aref y (+ (* j channels) ch))
                              (coerce sum 'single-float)))))))))
          (%make-pcm-buffer sample-rate channels out-frames y)))))
