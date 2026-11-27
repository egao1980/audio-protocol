;;;; AU3 windows, frames and STFT: window formulas, symmetry and overlap-add,
;;;; frame counts and edges for each padding policy, and spectra of sines and
;;;; of the reference engine's notes.

(in-package #:audio-protocol/tests)

;;; Windows -------------------------------------------------------------------------

(defun window-formula (kind n size symmetric)
  "Point N of the window, straight from frames.lisp."
  (if (= size 1)
      1d0
      (let ((u (/ (* 2 (dpi) n) (if symmetric (1- size) size))))
        (ecase kind
          (:rectangular 1d0)
          (:hann (- 0.5d0 (* 0.5d0 (cos u))))
          (:hamming (- 0.54d0 (* 0.46d0 (cos u))))
          (:blackman (+ (- 0.42d0 (* 0.5d0 (cos u))) (* 0.08d0 (cos (* 2 u)))))))))

(deftest-parametrize windows-follow-their-formulas
    ((kind symmetric)
     :ids ("rectangular" "rectangular-symmetric" "hann" "hann-symmetric"
           "hamming" "hamming-symmetric" "blackman" "blackman-symmetric")
     (:rectangular nil) (:rectangular t) (:hann nil) (:hann t)
     (:hamming nil) (:hamming t) (:blackman nil) (:blackman t))
  (dolist (size '(1 2 3 4 7 8 16 255 256 1024))
    (let ((w (dsp:make-window kind size :symmetric symmetric)))
      (ok (typep w +double-vector+))
      (ok (= size (length w)))
      (ok (loop for n below size
                always (approx= (window-formula kind n size symmetric) (aref w n) 1d-12))
          (format nil "~(~A~) ~:[periodic~;symmetric~] ~D" kind symmetric size)))))

(deftest window-edge-sizes
  (dolist (kind dsp:+window-kinds+)
    (ok (equalp #() (dsp:make-window kind 0)) (format nil "~(~A~) of 0: empty" kind))
    (ok (equalp #(1d0) (dsp:make-window kind 1)) (format nil "~(~A~) of 1: #(1d0)" kind))
    (ok (equalp #(1d0) (dsp:make-window kind 1 :symmetric t))))
  (ok (equal '(:rectangular :hann :hamming :blackman) dsp:+window-kinds+)))

(deftest window-symmetry
  (dolist (kind '(:hann :hamming :blackman))
    (dolist (size '(5 8 33 512))
      (let ((symmetric (dsp:make-window kind size :symmetric t))
            (periodic (dsp:make-window kind size))
            (longer (dsp:make-window kind (1+ size) :symmetric t)))
        (ok (loop for n below size
                  always (approx= (aref symmetric n) (aref symmetric (- size 1 n)) 1d-15))
            (format nil "symmetric ~(~A~) ~D: w(n) = w(N-1-n)" kind size))
        (ok (loop for n from 1 below size
                  always (approx= (aref periodic n) (aref periodic (- size n)) 1d-15))
            (format nil "periodic ~(~A~) ~D: w(n) = w(N-n)" kind size))
        (ok (within periodic (subseq longer 0 size) 1d-15)
            "periodic N is the first N points of symmetric N+1")))))

(deftest window-sums
  (flet ((sum (w) (reduce #'+ w)))
    (dolist (n '(8 64 512 1000))
      (ok (approx= (/ n 2) (sum (dsp:make-window :hann n)) 1d-9) (format nil "Hann ~D: N/2" n))
      (ok (approx= (* 0.54d0 n) (sum (dsp:make-window :hamming n)) 1d-9))
      (ok (approx= (* 0.42d0 n) (sum (dsp:make-window :blackman n)) 1d-9))
      (ok (approx= (/ (1- n) 2) (sum (dsp:make-window :hann n :symmetric t)) 1d-9)
          "symmetric Hann: (N-1)/2")
      (ok (= n (sum (dsp:make-window :rectangular n)))))))

(deftest-parametrize periodic-windows-overlap-add-flat
    ((kind size hop level)
     :ids ("hann-half" "hann-quarter" "hamming-half" "blackman-third")
     (:hann 512 256 1d0) (:hann 512 128 2d0) (:hamming 512 256 1.08d0) (:blackman 384 128 1.26d0))
  (let ((w (dsp:make-window kind size))
        (total (make-array (* 8 size) :initial-element 0d0)))
    (loop for start from 0 by hop
          while (<= (+ start size) (length total))
          do (dotimes (j size) (incf (aref total (+ start j)) (aref w j))))
    (ok (loop for n from size below (* 7 size) always (approx= level (aref total n) 1d-12))
        (format nil "~(~A~) at ~D/~D sums to ~A" kind hop size level))))

(deftest make-window-argument-types
  (ok (signals (dsp:make-window :triangle 8) 'type-error))
  (ok (signals (dsp:make-window :hann -1) 'type-error))
  (ok (signals (dsp:make-window :hann 8.0) 'type-error)))

;;; Frames --------------------------------------------------------------------------

(deftest-parametrize frame-counts
    ((n size hop pad want)
     :rows '((0 4 2 :center 0) (1 4 2 :center 1) (10 4 2 :center 5) (11 4 2 :center 6)
             (16000 2048 256 :center 63) (16384 2048 256 :center 64) (5 3 7 :center 1)
             (7 3 7 :center 1) (8 3 7 :center 2)
             (0 4 2 :end 0) (1 4 2 :end 1) (4 4 2 :end 1) (5 4 2 :end 2) (10 4 2 :end 4)
             (11 4 2 :end 5) (16000 2048 256 :end 56) (10 3 7 :end 2) (11 3 7 :end 3)
             (0 4 2 :none 0) (3 4 2 :none 0) (4 4 2 :none 1) (5 4 2 :none 1) (6 4 2 :none 2)
             (11 4 2 :none 4) (16000 2048 256 :none 55) (10 3 7 :none 2)))
  (ok (= want (dsp:frame-count n size hop :pad pad))
      (format nil "~D samples, frames of ~D every ~D, ~(~S~): ~D" n size hop pad want)))

(deftest frame-count-and-start-defaults
  (ok (= 5 (dsp:frame-count 10 4 2)) "PAD defaults to :CENTER")
  (ok (= -2 (dsp:frame-start 0 4 2)) "centred frames start half a frame early")
  (ok (= 4 (dsp:frame-start 3 4 2)))
  (ok (= 6 (dsp:frame-start 3 4 2 :pad :end)))
  (ok (= 6 (dsp:frame-start 3 4 2 :pad :none)))
  (ok (= -2 (dsp:frame-start 0 5 2)) "odd sizes: floor(SIZE/2) before the centre")
  (ok (= 24576 (dsp:frame-start 100 2048 256)) "frame i of a centred STFT is centred on i * hop"))

(defun one-to (n) (coerce (loop for i from 1 to n collect i) 'vector))

(deftest-parametrize frames-and-their-edges
    ((n size hop pad want)
     :ids '("center" "end" "none" "center-odd" "center-sparse" "center-short" "end-exact"
           "empty")
     :rows '((11 4 3 :center ((0 0 1 2) (2 3 4 5) (5 6 7 8) (8 9 10 11)))
             (11 4 3 :end ((1 2 3 4) (4 5 6 7) (7 8 9 10) (10 11 0 0)))
             (11 4 3 :none ((1 2 3 4) (4 5 6 7) (7 8 9 10)))
             (5 5 2 :center ((0 0 1 2 3) (1 2 3 4 5) (3 4 5 0 0)))
             (11 2 5 :center ((0 1) (5 6) (10 11)))
             (2 8 4 :center ((0 0 0 0 1 2 0 0)))
             (10 4 3 :end ((1 2 3 4) (4 5 6 7) (7 8 9 10)))
             (0 4 2 :center ())))
  (let ((frames (dsp:frame-signal (one-to n) size hop :pad pad)))
    (ok (typep frames 'simple-vector))
    (ok (every (lambda (f) (typep f +double-vector+)) frames) "each frame a DOUBLE-VECTOR")
    (ok (equalp want (frames-as-lists frames)))
    (ok (= (length frames) (dsp:frame-count n size hop :pad pad)))))

(deftest frames-of-other-signals
  (let ((want (frames-as-lists (dsp:frame-signal (one-to 9) 4 2))))
    (ok (equalp want (frames-as-lists
                      (dsp:frame-signal (make-array 9 :element-type 'single-float
                                                      :initial-contents (map 'list #'float (one-to 9)))
                                        4 2)))
        "a SAMPLE-VECTOR")
    (ok (equalp want (frames-as-lists
                      (dsp:frame-signal (make-pcm-buffer :sample-rate 8000 :samples (one-to 9)) 4 2)))
        "a mono PCM-BUFFER stands for its samples"))
  (ok (signals (dsp:frame-signal (make-pcm-buffer :channels 2 :frames 4) 2 1) 'audio-error)
      "a stereo buffer: DOWNMIX first")
  (ok (signals (dsp:frame-signal '(1 2 3) 2 1) 'type-error))
  (ok (signals (dsp:frame-signal (one-to 4) 0 1) 'type-error) "SIZE 0")
  (ok (signals (dsp:frame-signal (one-to 4) 2 0) 'type-error) "HOP 0")
  (ok (signals (dsp:frame-signal (one-to 4) 2 1 :pad :reflect) 'type-error))
  (ok (signals (dsp:frame-count 4 2 1 :pad :both) 'type-error))
  (ok (signals (dsp:frame-count -1 2 1) 'type-error)))

;;; STFT ----------------------------------------------------------------------------

(defun window-values (window size)
  (cond ((null window) (dsp:make-window :rectangular size))
        ((keywordp window) (dsp:make-window window size))
        (t window)))

(deftest stft-is-the-rfft-of-windowed-frames
  (let* ((*seed* 7)
         (x (random-reals 3000))
         (size 256)
         (hop 100))
    (dolist (pad dsp:+pad-policies+)
      (dolist (window (list :hann :blackman nil (dsp:make-window :hamming size :symmetric t)))
        (let* ((spectra (dsp:stft x size hop :window window :pad pad))
               (frames (dsp:frame-signal x size hop :pad pad))
               (w (window-values window size)))
          (ok (typep spectra 'simple-vector))
          (ok (= (length frames) (length spectra)))
          (ok (every (lambda (s) (and (typep s +complex-vector+) (= 129 (length s)))) spectra)
              "SIZE/2 + 1 bins a frame")
          (ok (loop for f across frames
                    for s across spectra
                    for wf = (map 'vector #'* f w)
                    always (within s (dsp:rfft wf) (* 1d-11 (abs-sum wf))))
              (format nil "~(~S~), window ~A" pad (if (vectorp window) "vector" window))))))
    (ok (within (aref (dsp:stft x size hop) 5) (aref (dsp:stft x size hop :window :hann :pad :center) 5)
                0)
        "defaults: :HANN, :CENTER")))

(deftest stft-of-a-bin-centred-sine
  ;; 1000 Hz at 16 kHz in 512 points is bin 32. A periodic Hann window gives
  ;; |X_32| = A N / 4, |X_31| = |X_33| = A N / 8, and nothing elsewhere.
  (let* ((x (sine-vector 16000 16000 1000 :amplitude 0.5d0))
         (spectra (dsp:stft x 512 128))
         (bad '()))
    (ok (= 125 (length spectra)))
    (loop for i from 4 below (- (length spectra) 4)
          for m = (dsp:magnitude (aref spectra i))
          unless (and (approx= 64 (aref m 32) 1d-3)
                      (approx= 32 (aref m 31) 1d-3)
                      (approx= 32 (aref m 33) 1d-3)
                      (loop for k below 257 always (or (<= 31 k 33) (< (aref m k) 1d-3))))
            do (push i bad))
    (ok (null bad) (report bad))
    (ok (= 1000 (dsp:bin-frequency 32 512 16000)))))

(deftest stft-peaks-at-the-notes-of-a-scale
  ;; The reference engine's scale at 44.1 kHz; a 4096-point frame in the
  ;; middle of each note (0.1875 s in) peaks at the bin nearest the note.
  (let* ((buffer (render-midi (sine-engine) (scale-smf) :sample-rate 44100 :channels 1))
         (spectra (dsp:stft buffer 4096 441)))
    (ok (= (ceiling (pcm-buffer-frames buffer) 441) (length spectra)))
    (loop for key in +c-major+
          for i from 0
          for m = (dsp:magnitude (aref spectra (round (+ (* 22050 i) 8269) 441)))
          for want = (round (* (note-frequency key) 4096) 44100)
          do (ok (= want (position (reduce #'max m) m))
                 (format nil "key ~D (~,1F Hz) peaks at bin ~D" key (note-frequency key) want)))))

(deftest stft-size-restarts
  (let* ((*seed* 9)
         (x (random-reals 2000))
         (c (caught (lambda () (dsp:stft x 400 160)) 'dsp:fft-size-error)))
    (ok (typep c 'dsp:fft-size-error) "400 points: FFT-SIZE-ERROR")
    (ok (and (typep c 'dsp:fft-size-error) (= 400 (dsp:fft-size-error-size c))))
    (ok (equal '(use-value dsp:zero-pad)
               (restarts-offered (lambda () (dsp:stft x 400 160)) 'dsp:fft-size-error
                                 '(use-value dsp:zero-pad))))
    (let ((padded (invoking 'dsp:fft-size-error 'dsp:zero-pad (lambda () (dsp:stft x 400 160))))
          (frames (dsp:frame-signal x 400 160))
          (w (dsp:make-window :hann 400)))
      (ok (= (length frames) (length padded)) "frames of 400 samples")
      (ok (every (lambda (s) (= 257 (length s))) padded) "512 points: 257 bins")
      (ok (loop for f across frames
                for s across padded
                for wf = (concatenate 'vector (map 'vector #'* f w)
                                      (make-array 112 :initial-element 0d0))
                always (within s (dsp:rfft wf) (* 1d-11 (abs-sum wf))))
          "each windowed frame padded to 512"))
    (let ((other (invoking 'dsp:fft-size-error 'use-value (lambda () (dsp:stft x 400 160)) 512))
          (want (dsp:stft x 512 160)))
      (ok (= (length want) (length other)) "USE-VALUE: another SIZE")
      (ok (loop for a across want for b across other always (within a b 0))))
    (let ((seen 0))
      (handler-bind ((dsp:fft-size-error (lambda (c) (incf seen) (dsp:zero-pad c))))
        (dsp:stft x 400 160))
      (ok (= 1 seen) "checked once per call, not once per frame"))))

(deftest stft-arguments
  (let ((x (sine-vector 1000 8000 440)))
    (ok (signals (dsp:stft x 256 128 :window (dsp:make-window :hann 255)) 'audio-error)
        "a window vector of another length")
    (ok (signals (dsp:stft x 256 128 :window :kaiser) 'type-error))
    (ok (signals (dsp:stft (make-pcm-buffer :channels 2 :frames 1000) 256 128) 'audio-error))
    (ok (signals (dsp:stft x 0 128) 'type-error))
    (ok (= 8 (length (dsp:stft (make-pcm-buffer :sample-rate 8000 :samples x) 256 128)))
        "a mono buffer")))

(deftest magnitude-and-bin-frequency
  (let ((m (dsp:magnitude (vector #C(3 4) -5 0d0 #C(0d0 -2d0)))))
    (ok (typep m +double-vector+))
    (ok (within m #(5 5 0 2) 1d-15)))
  (ok (eql 1000 (dsp:bin-frequency 32 512 16000)))
  (ok (eql 11025/1024 (dsp:bin-frequency 1 4096 44100)) "exact rationals")
  (ok (eql 0 (dsp:bin-frequency 0 512 16000)))
  (ok (signals (dsp:bin-frequency -1 512 16000) 'type-error))
  (ok (signals (dsp:bin-frequency 1 0 16000) 'type-error))
  (ok (signals (dsp:bin-frequency 1 512 16000.0) 'type-error))
  (ok (signals (dsp:magnitude '(1 2)) 'type-error)))
