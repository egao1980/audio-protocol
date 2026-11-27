;;;; AU3 FFT: against the direct DFT, the inverse, Parseval, the real FFT,
;;;; argument types, and FFT-SIZE-ERROR with its restarts.
;;;; Tolerance (fft.lisp): 1e-11 * S, S the sum of |x_n| (|X_k| / N for IFFT).

(in-package #:audio-protocol/tests)

(deftest power-of-two-helpers
  (ok (equal '(nil t t nil t nil nil t nil)
             (mapcar #'dsp:power-of-two-p '(0 1 2 3 4 6 1023 1024 1025))))
  (ok (equal '(1 1 2 4 4 8 1024 1024 2048 65536)
             (mapcar #'dsp:next-power-of-two '(0 1 2 3 4 5 1000 1024 1025 40000))))
  (ok (signals (dsp:next-power-of-two -1) 'type-error))
  (ok (signals (dsp:next-power-of-two 2.0) 'type-error))
  (ok (signals (dsp:power-of-two-p -4) 'type-error))
  (ok (signals (dsp:power-of-two-p 1/2) 'type-error)))

(deftest-parametrize fft-matches-the-dft
    ((n) (1) (2) (4) (8) (16) (32) (64) (256) (1024))
  (let* ((*seed* (+ 1000 n))
         (x (random-complexes n))
         (got (dsp:fft x)))
    (ok (typep got +complex-vector+) "a fresh COMPLEX-VECTOR")
    (ok (within got (direct-dft x) (* 1d-11 (abs-sum x)))
        (format nil "~D points within 1e-11 * S of the DFT" n))))

(deftest-parametrize ifft-matches-the-inverse-dft
    ((n) (1) (2) (8) (64) (512))
  (let* ((*seed* (+ 2000 n))
         (spectrum (random-complexes n))
         (got (dsp:ifft spectrum)))
    (ok (typep got +complex-vector+))
    (ok (within got (direct-idft spectrum) (* 1d-11 (/ (abs-sum spectrum) n))))))

(deftest-parametrize ifft-inverts-fft
    ((n) (1) (2) (16) (1024) (4096))
  (let* ((*seed* (+ 3000 n))
         (x (random-complexes n)))
    (ok (within (dsp:ifft (dsp:fft x)) x (* 1d-11 (abs-sum x))) "(ifft (fft x)) is x")
    (ok (within (dsp:fft (dsp:ifft x)) x (* 1d-11 (abs-sum x))) "(fft (ifft x)) is x")))

(deftest-parametrize parseval
    ((n) (4) (64) (512) (4096))
  (let* ((*seed* (+ 4000 n))
         (x (random-reals n))
         (spectrum (dsp:fft x))
         (time-energy (energy x))
         (frequency-energy (/ (loop for v across spectrum sum (expt (abs v) 2)) n)))
    (ok (approx= time-energy frequency-energy (* 1d-11 time-energy))
        "sum |x|^2 = (1/N) sum |X|^2")))

(deftest known-transforms
  (ok (within (dsp:fft #(1 0 0 0 0 0 0 0)) (make-array 8 :initial-element 1) 1d-15)
      "an impulse at 0 is flat")
  (ok (within (dsp:fft #(1 1 1 1 1 1 1 1)) #(8 0 0 0 0 0 0 0) 1d-14)
      "a constant is all at bin 0")
  (ok (within (dsp:fft #(1 -1 1 -1 1 -1 1 -1)) #(0 0 0 0 8 0 0 0) 1d-14)
      "the alternating signal is all at N/2")
  (ok (within (dsp:fft #(0 1 0 0)) (vector 1 #C(0 -1) -1 #C(0 1)) 1d-15)
      "a delay of one is exp(-2 pi i k / N)")
  (let ((cosine (map 'vector (lambda (n) (cos (/ (* 2 (dpi) 3 n) 16))) (loop for n below 16 collect n)))
        (want (make-array 16 :initial-element 0)))
    (setf (aref want 3) 8 (aref want 13) 8)
    (ok (within (dsp:fft cosine) want 1d-13) "cos at bin 3: N/2 at bins 3 and 13")))

(deftest fft-accepts-any-vector-of-numbers
  (let ((want (dsp:fft (vector 1d0 2d0 3d0 4d0))))
    (ok (within want (direct-dft #(1 2 3 4)) 1d-14))
    (ok (within (dsp:fft #(1 2 3 4)) want 1d-15) "integers")
    (ok (within (dsp:fft (vector 1/2 2 3 4)) (dsp:fft (vector 0.5d0 2 3 4)) 1d-15) "rationals")
    (ok (within (dsp:fft (make-array 4 :element-type 'single-float
                                       :initial-contents '(1.0 2.0 3.0 4.0)))
                want 1d-15)
        "single-floats")
    (ok (within (dsp:fft (make-array 4 :adjustable t :initial-contents '(1 2 3 4))) want 1d-15)
        "an adjustable vector")
    (ok (within (dsp:fft (make-array 6 :fill-pointer 4 :initial-contents '(1 2 3 4 5 6)))
                want 1d-15)
        "a fill pointer: its 4 active elements")
    (ok (within (dsp:fft (vector #C(1 0) 2 3d0 4.0)) want 1d-15) "mixed numbers")
    (ok (within (dsp:ifft #(1 2 3 4)) (dsp:ifft (vector 1d0 2d0 3d0 4d0)) 1d-15))))

(deftest fft-results-are-fresh
  (let* ((x (make-array 1 :element-type '(complex double-float)
                          :initial-element #C(2d0 -1d0)))
         (spectrum (dsp:fft x))
         (back (dsp:ifft x)))
    (ok (not (eq x spectrum)) "one point: a fresh vector, not the input")
    (ok (not (eq x back)))
    (ok (equalp x spectrum) "the transform of one point is itself")
    (ok (equalp x back)))
  (let* ((x (make-array 4 :element-type 'double-float :initial-contents '(1d0 2d0 3d0 4d0)))
         (c (map +complex-vector+ (lambda (v) (complex v 1d0)) x))
         (x-copy (copy-seq x))
         (c-copy (copy-seq c)))
    (dsp:fft x) (dsp:ifft c) (dsp:rfft x) (dsp:fft c)
    (ok (equalp x x-copy) "the inputs aren't modified")
    (ok (equalp c c-copy))))

(deftest-parametrize rfft-is-the-half-spectrum
    ((n) (1) (2) (4) (8) (64) (1024) (4096))
  (let* ((*seed* (+ 5000 n))
         (x (random-reals n))
         (half (dsp:rfft x))
         (bins (1+ (floor n 2))))
    (ok (typep half +complex-vector+))
    (ok (= bins (length half)) (format nil "~D points: ~D bins" n bins))
    (ok (within half (subseq (dsp:fft x) 0 bins) (* 1d-11 (abs-sum x)))
        "bins 0 .. N/2 of the complex FFT")
    (when (<= n 1024)
      (ok (within half (subseq (direct-dft x) 0 bins) (* 1d-11 (abs-sum x)))
          "and of the direct DFT"))))

(deftest rfft-of-other-reals
  (let ((want (dsp:rfft (vector 1d0 -2d0 3d0 0.5d0))))
    (ok (within (dsp:rfft #(1 -2 3 1/2)) want 1d-15))
    (ok (within (dsp:rfft (make-array 4 :element-type 'single-float
                                        :initial-contents '(1.0 -2.0 3.0 0.5)))
                want 1d-15))
    (ok (within want (vector 5/2 #C(-2 5/2) 11/2) 1d-15)
        "the values: 2.5, -2+2.5i, 5.5")))

(deftest fft-argument-types
  (ok (signals (dsp:fft '(1 2)) 'type-error) "a list")
  (ok (signals (dsp:ifft '(1 2)) 'type-error))
  (ok (signals (dsp:rfft '(1 2)) 'type-error))
  (ok (signals (dsp:fft (vector 1 :two)) 'type-error) "a non-number")
  (ok (signals (dsp:rfft (vector 1 #C(0 1))) 'type-error) "a complex element of a real FFT")
  (ok (signals (dsp:rfft "ab") 'type-error)))

;;; Sizes that are not powers of two ---------------------------------------------

(deftest-parametrize fft-size-error-is-signalled
    ((fn input) :ids ("fft" "ifft" "rfft" "fft-empty" "rfft-empty" "fft-1000")
     (#'dsp:fft #(1 2 3))
     (#'dsp:ifft #(1 2 3 4 5 6))
     (#'dsp:rfft #(1 2 3 4 5))
     (#'dsp:fft #())
     (#'dsp:rfft #())
     (#'dsp:fft (make-array 1000 :initial-element 0d0)))
  (let ((c (caught (lambda () (funcall fn input)) 'dsp:fft-size-error)))
    (ok (typep c 'dsp:fft-size-error))
    (ok (typep c 'audio-error) "an AUDIO-ERROR")
    (ok (and (typep c 'dsp:fft-size-error) (eql (length input) (dsp:fft-size-error-size c)))
        "SIZE is the length")
    (ok (and (typep c 'dsp:fft-size-error) (search "power of two" (princ-to-string c)))
        "the report says why")
    (ok (equal '(use-value dsp:zero-pad)
               (restarts-offered (lambda () (funcall fn input)) 'dsp:fft-size-error
                                 '(use-value dsp:zero-pad)))
        "restarts USE-VALUE and ZERO-PAD")))

(deftest zero-pad-restart
  (let ((padded (invoking 'dsp:fft-size-error 'dsp:zero-pad (lambda () (dsp:fft #(1 2 3))))))
    (ok (= 4 (length padded)))
    (ok (within padded (dsp:fft #(1 2 3 0)) 1d-15) "transforms the input padded to 4"))
  (let ((padded (invoking 'dsp:fft-size-error 'dsp:zero-pad (lambda () (dsp:ifft #(1 2 3 4 5))))))
    (ok (within padded (dsp:ifft #(1 2 3 4 5 0 0 0)) 1d-15)))
  (let ((padded (invoking 'dsp:fft-size-error 'dsp:zero-pad (lambda () (dsp:rfft #(1 2 3 4 5))))))
    (ok (= 5 (length padded)) "8 points: 5 bins")
    (ok (within padded (dsp:rfft #(1 2 3 4 5 0 0 0)) 1d-15)))
  (let ((padded (invoking 'dsp:fft-size-error 'dsp:zero-pad (lambda () (dsp:fft #())))))
    (ok (within padded #(0) 0) "nothing pads to one zero"))
  (let ((padded (handler-bind ((dsp:fft-size-error #'dsp:zero-pad))
                  (dsp:fft (make-array 1000 :initial-element 1d0)))))
    (ok (= 1024 (length padded)) "the ZERO-PAD function as a handler")
    (ok (approx= 1000 (realpart (aref padded 0)) 1d-9)))
  (ok (null (dsp:zero-pad)) "no restart: NIL"))

(deftest use-value-restart
  (let ((other (invoking 'dsp:fft-size-error 'use-value (lambda () (dsp:fft #(1 2 3))) #(5 6))))
    (ok (within other (dsp:fft #(5 6)) 1d-15) "transforms the value instead"))
  (let ((other (invoking 'dsp:fft-size-error 'use-value (lambda () (dsp:rfft #(1 2 3)))
                         #(1 1 1 1))))
    (ok (within other #(4 0 0) 1d-15)))
  (let* ((seen 0)
         (result (handler-bind ((dsp:fft-size-error
                                  (lambda (c)
                                    (incf seen)
                                    (if (= seen 1)
                                        (use-value #(1 2 3 4 5) c)
                                        (dsp:zero-pad c)))))
                   (dsp:fft #(1 2 3)))))
    (ok (= 2 seen) "the value is checked again")
    (ok (= 8 (length result)) "and the second restart pads it"))
  (let ((c (caught (lambda ()
                     (invoking 'dsp:fft-size-error 'use-value (lambda () (dsp:fft #(1 2 3)))
                               '(1 2)))
                   'type-error)))
    (ok (typep c 'type-error) "a list as the value is a TYPE-ERROR")))
