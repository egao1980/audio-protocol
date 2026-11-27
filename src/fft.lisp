;;;; AU3 FFT: radix-2 discrete Fourier transforms, complex and real.
;;;;
;;;; Package AUDIO-PROTOCOL/DSP (STACK-AUDIO/DSP), in the audio-protocol
;;;; system. Every DSP function computes in DOUBLE-FLOAT and returns fresh
;;;; arrays of two types:
;;;;   DOUBLE-VECTOR   (simple-array double-float (*))
;;;;   COMPLEX-VECTOR  (simple-array (complex double-float) (*))
;;;; Inputs are any VECTOR of the numbers stated (a list, or an element of
;;;; another type, is a TYPE-ERROR), read as DOUBLE-FLOATs or (COMPLEX
;;;; DOUBLE-FLOAT)s, and never modified. Below, pi is CL:PI as a DOUBLE-FLOAT
;;;; and i is the imaginary unit.
;;;;
;;;; Sizes. N, the length of the input, must be a power of two, 2^k with
;;;; k >= 0 (1, 2, 4, ...).
;;;;   (POWER-OF-TWO-P n) is true when N is one (0 is not).
;;;;   (NEXT-POWER-OF-TWO n) is the smallest power of two >= N, and 1 for
;;;;   N = 0.
;;;;   Both take a non-negative integer, else TYPE-ERROR.
;;;;
;;;; (FFT x) -> a fresh COMPLEX-VECTOR of length N. X is a VECTOR of NUMBERs:
;;;;     X_k = sum_{n=0}^{N-1} x_n exp(-2 pi i k n / N)          (no scaling)
;;;; (IFFT X) -> a fresh COMPLEX-VECTOR of length N. X is a VECTOR of NUMBERs:
;;;;     x_n = (1/N) sum_{k=0}^{N-1} X_k exp(+2 pi i k n / N)
;;;;   So (ifft (fft x)) is x again, and Parseval's theorem reads
;;;;     sum |x_n|^2 = (1/N) sum |X_k|^2.
;;;; (RFFT x) -> a fresh COMPLEX-VECTOR of length floor(N/2) + 1. X is a VECTOR
;;;;   of REALs (a complex element is a TYPE-ERROR). The result is the half
;;;;   spectrum X_0 .. X_{floor(N/2)} of (FFT x); the other bins mirror it,
;;;;   X_{N-k} = conj(X_k), because x is real. An implementation may compute
;;;;   it through a complex FFT of N/2 points.
;;;;
;;;; Accuracy. Each output is within 1e-11 * S of the exact transform. S is
;;;; the sum of |x_n| for FFT and RFFT, and (1/N) times the sum of |X_k| for
;;;; IFFT. A radix-2 transform with twiddles from exact angles, cis(-2 pi j /
;;;; N), gets about 1e-15 * S; a twiddle recurrence about N * 1e-16 * S, so
;;;; take twiddles from exact angles (or a table of them) beyond 2^16 points.
;;;; The cost is O(N log N).
;;;;
;;;; Sizes that are not powers of two. FFT, IFFT and RFFT check the types, then
;;;; N. When N is not a power of two (0 included), they signal FFT-SIZE-ERROR
;;;; (SIZE N) with ERROR inside two restarts:
;;;;   ZERO-PAD   Transform the input padded at the end with zeros to
;;;;              (next-power-of-two N) elements. The result has that length,
;;;;              or half of it plus one for RFFT.
;;;;   USE-VALUE  Transform the value instead, a VECTOR of the same kind. It is
;;;;              checked again from the start, so another bad size signals
;;;;              again.
;;;; The function ZERO-PAD invokes the restart of that name and returns NIL
;;;; when there is none, as CL:USE-VALUE does. STFT (frames.lisp) checks its
;;;; frame size the same way, once per call.

(in-package #:audio-protocol/dsp)

(deftype double-vector ()
  "A fresh real result of the DSP functions."
  '(simple-array double-float (*)))

(deftype complex-vector ()
  "A fresh complex result of the DSP functions (spectra)."
  '(simple-array (complex double-float) (*)))

(define-condition fft-size-error (audio-error)
  ((size :initarg :size :initform nil :reader fft-size-error-size
         :documentation "The length that is not a power of two."))
  (:documentation "An FFT of a length that is not a power of two (FFT, IFFT,
RFFT, STFT). Restarts: ZERO-PAD and USE-VALUE.")
  (:report (lambda (c s)
             (format s "FFT size ~D is not a power of two~@[ (~A)~]"
                     (fft-size-error-size c)
                     (audio-error-reason c)))))

(defun zero-pad (&optional condition)
  "Invoke the ZERO-PAD restart (FFT-SIZE-ERROR: pad to the next power of two).
Return NIL when there is no such restart."
  (let ((restart (find-restart 'zero-pad condition)))
    (when restart (invoke-restart restart))))


;;; Input coercion. Each helper returns a fresh typed vector that only its
;;; caller holds, so the code below may write into it. AREF below LENGTH
;;; honours a fill pointer.

(defun %real-input (x)
  "A fresh DOUBLE-VECTOR of the reals of the vector X, else TYPE-ERROR."
  (check-type x vector)
  (let* ((n (length x))
         (v (make-array n :element-type 'double-float)))
    (dotimes (i n v)
      (let ((e (aref x i)))
        (unless (realp e)
          (error 'type-error :datum e :expected-type 'real))
        (setf (aref v i) (float e 1d0))))))

(defun %complex-input (x)
  "A fresh COMPLEX-VECTOR of the numbers of the vector X, else TYPE-ERROR."
  (check-type x vector)
  (let* ((n (length x))
         (v (make-array n :element-type '(complex double-float))))
    (dotimes (i n v)
      (let ((e (aref x i)))
        (setf (aref v i)
              (typecase e
                (real (complex (float e 1d0) 0d0))
                (complex (complex (float (realpart e) 1d0) (float (imagpart e) 1d0)))
                (t (error 'type-error :datum e :expected-type 'number))))))))

(declaim (inline %dvec))               ; so callers see a DOUBLE-VECTOR
(defun %dvec (n)
  "A fresh DOUBLE-VECTOR of N zeros."
  (make-array n :element-type 'double-float :initial-element 0d0))

(defun %padded (v points &optional (end (length v)))
  "A fresh DOUBLE-VECTOR of POINTS elements: V's first END values, then zeros."
  (declare (type double-vector v))
  (replace (%dvec points) v :end2 end))

;;; Sizes ------------------------------------------------------------------------

(defun power-of-two-p (n)
  "True when the integer N is 2^k for some k >= 0."
  (check-type n (integer 0))
  (if (and (plusp n) (zerop (logand n (1- n)))) t nil))

(defun next-power-of-two (n)
  "The smallest power of two >= N (a non-negative integer); 1 for 0."
  (check-type n (integer 0))
  (if (<= n 1) 1 (ash 1 (integer-length (1- n)))))

(defun %read-replacement ()
  "For USE-VALUE's :INTERACTIVE: read a form from *QUERY-IO*, evaluate it, and
return its value in a list."
  (format *query-io* "~&Form for the value to use instead (evaluated): ")
  (force-output *query-io*)
  (list (eval (read *query-io*))))

(defun %settle-size (value check size-of reason)
  "Check VALUE and its size until the size is a power of two. CHECK returns the
value coerced (or signals a TYPE-ERROR), and SIZE-OF gives the size of that.
When the size is not a power of two, signal FFT-SIZE-ERROR inside the restarts
ZERO-PAD and USE-VALUE. A USE-VALUE value is checked again from the start.
Return the checked value and the number of points to transform: the size, or
the next power of two after ZERO-PAD."
  (loop
    (let* ((checked (funcall check value))
           (n (funcall size-of checked)))
      (when (power-of-two-p n)
        (return (values checked n)))
      (restart-case (error 'fft-size-error :size n :reason reason)
        (zero-pad ()
          :report (lambda (stream)
                    (format stream "Pad with zeros to ~D points." (next-power-of-two n)))
          (return (values checked (next-power-of-two n))))
        (use-value (new)
          :report "Use another value instead; it is checked again."
          :interactive %read-replacement
          (setf value new))))))

;;; The kernel ---------------------------------------------------------------------
;;; Complex data are two DOUBLE-VECTORs, RE and IM, so the butterflies run on
;;; unboxed doubles on any Lisp. COMPLEX-VECTORs appear only at the boundary.

(defun %twiddles (n sign)
  "The twiddles of an N-point transform (N a power of two; SIGN -1 forward, +1
inverse) as two fresh DOUBLE-VECTORs of N - 1 entries, the cosines and the
signed sines, laid out by butterfly stage. The stage whose butterflies are
HALF apart (HALF = 1, 2, 4, ... N/2) holds cis(SIGN 2 pi k / (2 HALF)) for k
below HALF from entry HALF - 1 on, so the kernel reads each stage in order.
The last stage is cis(SIGN 2 pi j / N), j below N/2, each from its own angle
(never a recurrence). The other stages copy every (N / 2 HALF)-th of them."
  (declare (type fixnum n))
  (let* ((half-n (floor n 2))
         (last (1- half-n))               ; where the stage HALF = N/2 starts
         (cs (%dvec (max 0 (1- n))))
         (sn (%dvec (max 0 (1- n))))
         (two-pi (* 2d0 (float pi 1d0)))
         (points (float n 1d0))
         (negate (minusp sign)))
    (declare (type fixnum half-n last))
    (dotimes (j half-n)
      (let ((u (/ (* two-pi (float j 1d0)) points)))
        (setf (aref cs (+ last j)) (cos u)
              (aref sn (+ last j)) (if negate (- (sin u)) (sin u)))))
    (do ((half 1 (* 2 half)))
        ((>= (* 2 half) n) (values cs sn))
      (declare (type fixnum half))
      (let ((stride (floor n (* 2 half))))
        (declare (type fixnum stride))
        (do ((k 0 (1+ k))
             (from last (+ from stride))
             (to (1- half) (1+ to)))
            ((>= k half))
          (declare (type fixnum k from to))
          (setf (aref cs to) (aref cs from)
                (aref sn to) (aref sn from)))))))

(defun %fft-split! (re im cs sn)
  "Transform RE + i IM in place: iterative radix 2, decimation in time, with
the tables CS and SN of %TWIDDLES for (length RE) points, a power of two.
Return RE and IM. Only called on arrays its caller allocated."
  (declare (type double-vector re im cs sn))
  (let ((n (length re)))
    (declare (type fixnum n))
    ;; Bit-reversal permutation.
    (let ((j 0))
      (declare (type fixnum j))
      (loop for i of-type fixnum from 1 below n
            do (let ((bit (ash n -1)))
                 (declare (type fixnum bit))
                 (loop while (logtest j bit)
                       do (setf j (logxor j bit)
                                bit (ash bit -1)))
                 (setf j (logior j bit))
                 (when (< i j)
                   (rotatef (aref re i) (aref re j))
                   (rotatef (aref im i) (aref im j))))))
    ;; Stage HALF = 1: the twiddle is 1.
    (do ((a 0 (+ a 2)))
        ((>= (1+ a) n))
      (declare (type fixnum a))
      (let ((ar (aref re a))
            (ai (aref im a))
            (br (aref re (1+ a)))
            (bi (aref im (1+ a))))
        (setf (aref re a) (+ ar br)
              (aref im a) (+ ai bi)
              (aref re (1+ a)) (- ar br)
              (aref im (1+ a)) (- ai bi))))
    ;; Stages HALF = 2, 4, ... N/2, block by block (LEN = 2 HALF points), so
    ;; both the data and the stage's twiddles (from HALF - 1) are read in order.
    (do ((half 2 (* 2 half)))
        ((>= half n))
      (declare (type fixnum half))
      (let ((len (* 2 half))
            (offset (1- half)))
        (declare (type fixnum len offset))
        (do ((start 0 (+ start len)))
            ((>= start n))
          (declare (type fixnum start))
          (dotimes (k half)
            (let* ((wr (aref cs (+ offset k)))
                   (wi (aref sn (+ offset k)))
                   (a (+ start k))
                   (b (+ a half))
                   (br (aref re b))
                   (bi (aref im b))
                   (tr (- (* wr br) (* wi bi)))
                   (ti (+ (* wr bi) (* wi br)))
                   (ar (aref re a))
                   (ai (aref im a)))
              (declare (type fixnum a b))
              (setf (aref re a) (+ ar tr)
                    (aref im a) (+ ai ti)
                    (aref re b) (- ar tr)
                    (aref im b) (- ai ti)))))))
    (values re im)))

(defun %join (re im count)
  "A fresh COMPLEX-VECTOR of the first COUNT values RE + i IM."
  (declare (type double-vector re im))
  (let ((out (make-array count :element-type '(complex double-float))))
    (dotimes (k count out)
      (setf (aref out k) (complex (aref re k) (aref im k))))))

(defun %complex-transform (v points sign)
  "The POINTS-point transform of the COMPLEX-VECTOR V padded with zeros to
POINTS (SIGN -1 forward, +1 inverse without the scaling), as two fresh
DOUBLE-VECTORs RE and IM."
  (declare (type complex-vector v))
  (let ((re (%dvec points))
        (im (%dvec points)))
    (dotimes (i (length v))
      (setf (aref re i) (realpart (aref v i))
            (aref im i) (imagpart (aref v i))))
    (multiple-value-bind (cs sn) (%twiddles points sign)
      (%fft-split! re im cs sn))))

(defun %half-spectrum (re cs sn)
  "Bins 0 .. N/2 of the transform of the reals RE, a fresh DOUBLE-VECTOR of N
points that the caller gives up, as a fresh COMPLEX-VECTOR. CS and SN are the
forward tables of %TWIDDLES for N points."
  (let ((im (%dvec (length re))))
    (%fft-split! re im cs sn)
    (%join re im (1+ (floor (length re) 2)))))

;;; Transforms ---------------------------------------------------------------------

(defun fft (x)
  "The discrete Fourier transform of X, a vector of N numbers (N a power of
two), as a fresh COMPLEX-VECTOR. No scaling. See fft.lisp."
  (multiple-value-bind (v points) (%settle-size x #'%complex-input #'length "fft input")
    (multiple-value-bind (re im) (%complex-transform v points -1)
      (%join re im points))))

(defun ifft (spectrum)
  "The inverse transform of SPECTRUM, a vector of N numbers (N a power of
two), scaled by 1/N, as a fresh COMPLEX-VECTOR. See fft.lisp."
  (multiple-value-bind (v points)
      (%settle-size spectrum #'%complex-input #'length "ifft input")
    (multiple-value-bind (re im) (%complex-transform v points +1)
      (declare (type double-vector re im))
      (let ((scale (/ 1d0 points)))     ; exact: POINTS is a power of two
        (dotimes (k points)
          (setf (aref re k) (* scale (aref re k))
                (aref im k) (* scale (aref im k)))))
      (%join re im points))))

(defun rfft (x)
  "Bins 0 .. floor(N/2) of the transform of X, a vector of N reals (N a power
of two), as a fresh COMPLEX-VECTOR. See fft.lisp."
  (multiple-value-bind (v points) (%settle-size x #'%real-input #'length "rfft input")
    (multiple-value-bind (cs sn) (%twiddles points -1)
      (%half-spectrum (if (= points (length v)) v (%padded v points)) cs sn))))
