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

(defun %au3-pending (name)
  "Contract stub for slice AU3. Every call is replaced by the implementation,
then this function is deleted."
  (error "audio-protocol AU3 pending: ~A" name))

(defun power-of-two-p (n)
  "True when the integer N is 2^k for some k >= 0."
  (declare (ignorable n))
  (%au3-pending 'power-of-two-p))

(defun next-power-of-two (n)
  "The smallest power of two >= N (a non-negative integer); 1 for 0."
  (declare (ignorable n))
  (%au3-pending 'next-power-of-two))

(defun fft (x)
  "The discrete Fourier transform of X, a vector of N numbers (N a power of
two), as a fresh COMPLEX-VECTOR. No scaling. See fft.lisp."
  (declare (ignorable x))
  (%au3-pending 'fft))

(defun ifft (spectrum)
  "The inverse transform of SPECTRUM, a vector of N numbers (N a power of
two), scaled by 1/N, as a fresh COMPLEX-VECTOR. See fft.lisp."
  (declare (ignorable spectrum))
  (%au3-pending 'ifft))

(defun rfft (x)
  "Bins 0 .. floor(N/2) of the transform of X, a vector of N reals (N a power
of two), as a fresh COMPLEX-VECTOR. See fft.lisp."
  (declare (ignorable x))
  (%au3-pending 'rfft))
