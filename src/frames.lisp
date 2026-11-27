;;;; AU3 windows, analysis frames, and the short-time Fourier transform.
;;;;
;;;; Signals. A SIGNAL is a VECTOR of REALs (a SAMPLE-VECTOR, a DOUBLE-VECTOR,
;;;; ...) or a PCM-BUFFER of one channel, which stands for its samples. A
;;;; PCM-BUFFER of more channels signals AUDIO-ERROR (DOWNMIX makes a mono
;;;; one). Anything else is a TYPE-ERROR. L is the signal's length in samples.
;;;;
;;;; Frames. An analysis frame is SIZE consecutive samples of a signal, read as
;;;; DOUBLE-FLOATs, with 0 at every position outside 0 .. L-1. (In the AU0
;;;; model a frame is one sample of every channel. A signal here is mono, so
;;;; both kinds of frame count samples.) SIZE and HOP are positive integers,
;;;; else TYPE-ERROR, and HOP may exceed SIZE. PAD is one of +PAD-POLICIES+,
;;;; else TYPE-ERROR. Frame i, counted from 0, starts at sample
;;;;     s_i = i * HOP - floor(SIZE / 2)    PAD :CENTER (the default)
;;;;     s_i = i * HOP                      PAD :END and :NONE
;;;; and each policy makes this many frames:
;;;;   :CENTER  Frame i is centred on sample i * HOP, and there is one frame
;;;;            for each i * HOP < L: ceiling(L / HOP) frames, 0 for L = 0.
;;;;            Both ends are padded with zeros as far as a frame reaches. So
;;;;            frame i sits at time i * HOP / rate, the convention of pitch
;;;;            trackers and of centred STFTs. (For an even SIZE, librosa and
;;;;            torch add one more frame when HOP divides L. Here every frame's
;;;;            centre is a sample of the signal.)
;;;;   :END     Frames from sample 0 up to the first one that reaches the last
;;;;            sample: 0 for L = 0, else 1 + ceiling(max(0, L - SIZE) / HOP).
;;;;            The tail of the last frame is padded with zeros.
;;;;   :NONE    Whole frames only: 0 when L < SIZE, else
;;;;            1 + floor((L - SIZE) / HOP). No padding. Trailing samples that
;;;;            don't fill a frame are left out.
;;;;
;;;; (FRAME-COUNT length size hop &key (pad :center)) -> the number of frames
;;;;   of a signal of LENGTH samples. LENGTH is a non-negative integer, else
;;;;   TYPE-ERROR.
;;;; (FRAME-START index size hop &key (pad :center)) -> s_INDEX, an integer
;;;;   (negative for the first frames under :CENTER). INDEX is a non-negative
;;;;   integer, else TYPE-ERROR; it isn't checked against a length.
;;;; (FRAME-SIGNAL signal size hop &key (pad :center)) -> a fresh SIMPLE-VECTOR
;;;;   of (frame-count L size hop :pad pad) fresh DOUBLE-VECTORs of SIZE
;;;;   elements each. Element j of frame i is sample s_i + j, or 0 outside the
;;;;   signal. Exact: each value is a sample converted to a DOUBLE-FLOAT.
;;;;
;;;; Windows. (MAKE-WINDOW kind size &key symmetric) -> a fresh DOUBLE-VECTOR
;;;;   of SIZE elements, SIZE a non-negative integer and KIND one of
;;;;   +WINDOW-KINDS+ (else TYPE-ERROR). With D = SIZE (periodic, the default)
;;;;   or D = SIZE - 1 (SYMMETRIC true), and u = 2 pi n / D for n = 0 .. SIZE-1:
;;;;     :RECTANGULAR  1
;;;;     :HANN         0.5  - 0.5  cos u
;;;;     :HAMMING      0.54 - 0.46 cos u
;;;;     :BLACKMAN     0.42 - 0.5  cos u + 0.08 cos 2u
;;;;   each within 1e-12 of that expression evaluated in DOUBLE-FLOAT.
;;;;   SIZE 1 gives #(1d0) for every kind, as numpy and scipy do (D would be 0
;;;;   or the window a lone 0); SIZE 0 gives an empty vector.
;;;;   Periodic is the default because analysis wants it. It is the first SIZE
;;;;   points of the symmetric window of SIZE + 1, so its overlap-add is flat:
;;;;   periodic Hann at a hop of SIZE/2 sums to exactly 1 (Hamming to 1.08),
;;;;   and Blackman at SIZE/3 sums to 1.26. Symmetric, w_n = w_{SIZE-1-n}, is
;;;;   the filter-design form, numpy's hanning, hamming and blackman. The
;;;;   coefficients are the classic truncated ones (0.54 and 0.46; 0.42, 0.5
;;;;   and 0.08), as numpy, scipy and RESAMPLE's kernel use.
;;;;
;;;; (STFT signal size hop &key (window :hann) (pad :center)) -> a fresh
;;;;   SIMPLE-VECTOR with one COMPLEX-VECTOR per frame of FRAME-SIGNAL: the
;;;;   RFFT of the frame multiplied element by element by the window. WINDOW
;;;;   is a kind of +WINDOW-KINDS+ (MAKE-WINDOW kind SIZE, periodic), a VECTOR
;;;;   of SIZE REALs (another length is an AUDIO-ERROR), or NIL (rectangular).
;;;;   Each spectrum has floor(SIZE/2) + 1 bins, and bin k is at
;;;;   (bin-frequency k SIZE rate) Hz. Each bin is within 1e-11 * S of that
;;;;   definition, S the sum of |frame_j * w_j|.
;;;;   SIZE must be a power of two. STFT checks the argument types, then SIZE,
;;;;   once, before anything is computed. When SIZE is not a power of two it
;;;;   signals FFT-SIZE-ERROR (SIZE) with ERROR inside two restarts:
;;;;     ZERO-PAD   Frames and window keep SIZE; each windowed frame is padded
;;;;                with zeros to F = (next-power-of-two SIZE) points before its
;;;;                RFFT. So each spectrum has F/2 + 1 bins, bin k at
;;;;                (bin-frequency k F rate): an FFT longer than the window,
;;;;                as for 25 ms frames at 16 kHz (400 samples, 512 points).
;;;;     USE-VALUE  Use the value, a positive integer, as SIZE. It is checked
;;;;                again (and a window vector must have that length).
;;;; (MAGNITUDE spectrum) -> a fresh DOUBLE-VECTOR of |X_k| for SPECTRUM, a
;;;;   VECTOR of NUMBERs.
;;;; (BIN-FREQUENCY k size sample-rate) -> k * SAMPLE-RATE / SIZE, an exact
;;;;   rational: the frequency in Hz of bin K of a SIZE-point transform. K is a
;;;;   non-negative integer, SIZE and SAMPLE-RATE positive integers, else
;;;;   TYPE-ERROR.

(in-package #:audio-protocol/dsp)

(defparameter +window-kinds+ '(:rectangular :hann :hamming :blackman)
  "The KIND values of MAKE-WINDOW and STFT's :WINDOW. Data.")

(defparameter +pad-policies+ '(:center :end :none)
  "The :PAD values of the framing functions. Data.")

;;; Argument checks and the framing arithmetic, shared with levels.lisp ------------

(defun %check-member (value list)
  "TYPE-ERROR unless VALUE is one of LIST."
  (unless (member value list)
    (error 'type-error :datum value :expected-type `(member ,@list))))

(defun %signal-samples (signal)
  "The samples of SIGNAL, a vector of reals or a one-channel PCM-BUFFER, as a
fresh DOUBLE-VECTOR. A buffer of more channels is an AUDIO-ERROR."
  (if (pcm-buffer-p signal)
      (let ((channels (pcm-buffer-channels signal)))
        (unless (= channels 1)
          (error 'audio-error
                 :reason (format nil "a signal is one channel, not ~D (downmix it first)"
                                 channels)))
        (%real-input (pcm-buffer-samples signal)))
      (%real-input signal)))

(defun %frame-count (length size hop pad)
  "The number of frames of LENGTH samples (the file header's formulas)."
  (values
   (ecase pad
     (:center (ceiling length hop))
     (:end (if (zerop length) 0 (1+ (ceiling (max 0 (- length size)) hop))))
     (:none (if (< length size) 0 (1+ (floor (- length size) hop)))))))

(defun %frame-start (index size hop pad)
  "s_INDEX: the sample at which frame INDEX starts."
  (if (eq pad :center)
      (- (* index hop) (floor size 2))
      (* index hop)))

(defun %window-value (kind m d)
  "Point M of the window KIND with denominator D, u = 2 pi M / D."
  (let ((u (/ (* 2d0 (float pi 1d0) (float m 1d0)) (float d 1d0))))
    (ecase kind
      (:hann (- 0.5d0 (* 0.5d0 (cos u))))
      (:hamming (- 0.54d0 (* 0.46d0 (cos u))))
      (:blackman (+ (- 0.42d0 (* 0.5d0 (cos u))) (* 0.08d0 (cos (* 2 u))))))))

(defun %frame-size (size)
  "SIZE as STFT's frame size, a positive integer, else TYPE-ERROR."
  (check-type size (integer 1))
  size)

;;; Windows, frames, STFT ------------------------------------------------------------

(defun make-window (kind size &key symmetric)
  "A fresh DOUBLE-VECTOR: the window KIND of SIZE points, periodic unless
SYMMETRIC. See frames.lisp."
  (%check-member kind +window-kinds+)
  (check-type size (integer 0))
  (let ((w (make-array size :element-type 'double-float :initial-element 1d0)))
    (when (and (> size 1) (not (eq kind :rectangular)))
      (let ((d (if symmetric (1- size) size)))
        ;; Point n reads the angle of min(n, D - n), so w(n) = w(D - n) holds
        ;; exactly, and u stays in 0 .. pi.
        (dotimes (n size)
          (setf (aref w n) (%window-value kind (min n (- d n)) d)))))
    w))

(defun frame-count (length size hop &key (pad :center))
  "How many frames FRAME-SIGNAL makes of LENGTH samples. See frames.lisp."
  (check-type length (integer 0))
  (check-type size (integer 1))
  (check-type hop (integer 1))
  (%check-member pad +pad-policies+)
  (%frame-count length size hop pad))

(defun frame-start (index size hop &key (pad :center))
  "The sample at which frame INDEX starts (negative before the signal)."
  (check-type index (integer 0))
  (check-type size (integer 1))
  (check-type hop (integer 1))
  (%check-member pad +pad-policies+)
  (%frame-start index size hop pad))

(defun frame-signal (signal size hop &key (pad :center))
  "A fresh SIMPLE-VECTOR of DOUBLE-VECTOR frames of SIGNAL, SIZE samples each,
HOP apart. See frames.lisp."
  (check-type signal (or vector pcm-buffer))
  (check-type size (integer 1))
  (check-type hop (integer 1))
  (%check-member pad +pad-policies+)
  (let* ((x (%signal-samples signal))
         (length (length x))
         (count (%frame-count length size hop pad))
         (frames (make-array count)))
    (declare (type double-vector x))
    (dotimes (i count frames)
      (let ((frame (%dvec size))
            (start (%frame-start i size hop pad)))
        (loop for j from (max 0 (- start)) below (min size (- length start))
              do (setf (aref frame j) (aref x (+ start j))))
        (setf (aref frames i) frame)))))

(defun stft (signal size hop &key (window :hann) (pad :center))
  "A fresh SIMPLE-VECTOR of half spectra, the RFFT of each windowed frame of
SIGNAL. See frames.lisp."
  ;; The argument types, then SIZE once (the restarts), then the values that
  ;; depend on the final SIZE, then the frames.
  (check-type signal (or vector pcm-buffer))
  (check-type size (integer 1))
  (check-type hop (integer 1))
  (%check-member pad +pad-policies+)
  (unless (or (null window) (vectorp window) (member window +window-kinds+))
    (error 'type-error :datum window
                       :expected-type `(or null vector (member ,@+window-kinds+))))
  (let ((given (and (vectorp window) (%real-input window)))
        (x (%signal-samples signal)))
    (declare (type double-vector x))
    (multiple-value-bind (frame-size points)
        (%settle-size size #'%frame-size #'identity "stft frame size")
      (when (and given (/= (length given) frame-size))
        (error 'audio-error
               :reason (format nil "a window of ~D points for frames of ~D"
                               (length given) frame-size)))
      (let* ((w (or given (make-window (or window :rectangular) frame-size)))
             (length (length x))
             (count (%frame-count length frame-size hop pad))
             (spectra (make-array count)))
        (declare (type double-vector w))
        (multiple-value-bind (cs sn) (%twiddles points -1)
          (dotimes (i count spectra)
            (let ((re (%dvec points))
                  (start (%frame-start i frame-size hop pad)))
              (declare (type double-vector re))
              (loop for j from (max 0 (- start)) below (min frame-size (- length start))
                    do (setf (aref re j) (* (aref x (+ start j)) (aref w j))))
              (setf (aref spectra i) (%half-spectrum re cs sn)))))))))

(defun magnitude (spectrum)
  "A fresh DOUBLE-VECTOR of the absolute values of SPECTRUM."
  (let* ((v (%complex-input spectrum))
         (out (%dvec (length v))))
    (declare (type complex-vector v))
    (dotimes (k (length v) out)
      (setf (aref out k) (abs (aref v k))))))

(defun bin-frequency (k size sample-rate)
  "K * SAMPLE-RATE / SIZE, exactly: the frequency of bin K."
  (check-type k (integer 0))
  (check-type size (integer 1))
  (check-type sample-rate (integer 1))
  (/ (* k sample-rate) size))
