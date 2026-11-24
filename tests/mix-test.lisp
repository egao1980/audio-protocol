;;;; AU0 mixing: the pan law, sums, gains, mute and solo, lengths, clipping,
;;;; mismatched inputs.

(in-package #:audio-protocol/tests)

;;; The pan law -------------------------------------------------------------------

(defun sine-law (pan)
  "(left right) of the constant-power sine law, computed here independently."
  (let ((p (float pan 1d0)))
    (list (sin (* (- 1 p) (/ (dpi) 4))) (sin (* (+ 1 p) (/ (dpi) 4))))))

(deftest mono-pan-ends-and-centre-are-exact
  (ok (equalp '(1d0 0d0) (multiple-value-list (pan-gains -1 1))))
  (ok (equalp '(0d0 1d0) (multiple-value-list (pan-gains 1 1))))
  (ok (equalp '(1d0 0d0) (multiple-value-list (pan-gains -1.0 1))))
  (multiple-value-bind (l r) (pan-gains 0 1)
    (ok (= l r))
    (ok (approx= (sqrt 0.5d0) l 1d-15))
    (ok (approx= -3.0103d0 (* 20 (log l 10)) 1d-4) "-3.01 dB per side")))

(deftest-parametrize mono-pan-is-the-sine-law
    ((pan) :rows '(-1 -3/4 -1/2 -1/4 -0.1d0 0 1/10 1/4 1/2 3/4 0.9 1))
  (multiple-value-bind (l r) (pan-gains pan 1)
    (ok (typep l 'double-float))
    (ok (typep r 'double-float))
    (ok (every-approx= (sine-law pan) (list l r) 1d-15))
    (ok (approx= 1d0 (+ (* l l) (* r r)) 1d-15) "constant power")))

(deftest mono-pan-moves-monotonically
  (let ((gains (loop for i from -10 to 10
                     collect (multiple-value-list (pan-gains (/ i 10) 1)))))
    (ok (every #'> (mapcar #'first gains) (mapcar #'first (rest gains))))
    (ok (every #'< (mapcar #'second gains) (mapcar #'second (rest gains))))))

(deftest-parametrize stereo-pan-is-balance
    ((pan left right) :rows '((-1 1 0) (-1/2 1 1/2) (0 1 1) (1/4 3/4 1) (1/2 1/2 1) (1 0 1)))
  (multiple-value-bind (l r) (pan-gains pan 2)
    (ok (= left l))
    (ok (= right r))
    (ok (typep l 'double-float))))

(deftest pan-argument-errors
  (ok (signals (pan-gains 2 1) 'type-error))
  (ok (signals (pan-gains -1.5 2) 'type-error))
  (ok (signals (pan-gains 0 3) 'type-error))
  (ok (signals (pan-gains :left 1) 'type-error)))

;;; Sums ------------------------------------------------------------------------

(defun stereo (&rest samples) (apply #'buffer 8000 2 samples))
(defun mono (&rest samples) (apply #'buffer 8000 1 samples))

(defun mixed (inputs &rest keys)
  "MIX with warnings muffled."
  (handler-bind ((samples-clipped #'muffle-warning))
    (apply #'mix inputs keys)))

(deftest one-stereo-input-is-unchanged
  (let ((b (stereo 0.5 -0.25 0.125 0.75 -1.0 1.0)))
    (ok (equalp b (mix (list b))))
    (ok (equalp b (mix (list b) :gains '(0) :pans '(0) :master 0)))))

(deftest one-mono-input-in-a-mono-mix-is-unchanged
  (let ((b (mono 0.5 -0.25 0.1)))
    (ok (equalp b (mix (list b) :channels 1)))
    (ok (equalp b (mix (list b) :channels 1 :pans '(1))) "pans are ignored in a mono mix")))

(deftest mono-into-stereo-uses-the-pan-law
  (let ((x '(0.5 -0.25 0.1)))
    (dolist (pan '(-1 -1/3 0 1/2 1))
      (multiple-value-bind (l r) (pan-gains pan 1)
        (ok (equalp (loop for v in x
                         nconc (list (single (* l (float (single v) 1d0)))
                                     (single (* r (float (single v) 1d0)))))
                   (samples (mix (list (apply #'mono x)) :pans (list pan)))))))))

(deftest stereo-balance-in-a-mix
  (ok (equalp (stereo 0.25 0.25 -0.125 0.125)
              (mix (list (stereo 0.5 0.25 -0.25 0.125)) :pans '(1/2) :gains '(0))))
  (ok (equalp (stereo 0.5 0.125 -0.25 0.0625)
              (mix (list (stereo 0.5 0.25 -0.25 0.125)) :pans '(-1/2))))
  (ok (equalp (stereo 0.0 0.25 0.0 0.125)
              (mix (list (stereo 0.5 0.25 -0.25 0.125)) :pans '(1)))))

(deftest stereo-into-mono-averages
  (ok (equalp (mono 0.375 -0.0625)
              (mix (list (stereo 0.5 0.25 -0.25 0.125)) :channels 1))))

(deftest two-inputs-sum-in-double-and-round-once
  (let* ((a (stereo 0.1 0.2 0.3 0.4))
         (b (stereo 1e-8 -0.2 0.7 -0.4))
         (want (map 'list (lambda (x y) (single (+ (float x 1d0) (float y 1d0))))
                    (pcm-buffer-samples a) (pcm-buffer-samples b))))
    (ok (equalp want (samples (mix (list a b)))))
    (ok (equalp want (samples (mix (list b a)))))))

(deftest gains-in-db
  (let ((b (stereo 0.5 -0.25 0.125 1.0)))
    (dolist (db '(-6 -12.5 3 0.5d0 -60))
      (ok (every-approx= (map 'list (lambda (x) (* (db-to-gain db) x)) (pcm-buffer-samples b))
                         (samples (mixed (list b) :gains (list db) :clip :none))
                         1d-6)))
    (ok (every-approx= (map 'list (lambda (x) (* (db-to-gain -9) x)) (pcm-buffer-samples b))
                       (samples (mix (list b) :gains '(-3) :master -6))
                       1d-6)
        "master and input gains multiply")))

(deftest stems-mix-as-their-buffers
  (let ((a (stereo 0.25 0.5)) (b (mono 0.125)))
    (ok (equalp (mix (list a b) :gains '(-3 0) :pans '(0 1/2))
                (mix (list (make-stem :name "A" :buffer a) (make-stem :name "B" :buffer b))
                     :gains '(-3 0) :pans '(0 1/2))))))

;;; Mute and solo --------------------------------------------------------------------

(defparameter +mute-solo-rows+
  '((() () (1 2 4))
    ((nil t nil) () (1 4))
    ((t t t) () ())
    (() (nil t nil) (2))
    (() (t nil t) (1 4))
    ((t nil nil) (t t nil) (2))
    ((nil t nil) (nil t nil) ())
    ((t nil nil) (nil nil t) (4))
    (() (t t t) (1 2 4)))
  "(mute solo heard): three DC inputs worth 1/8, 2/8 and 4/8; HEARD lists the eighths.")

(deftest-parametrize mute-and-solo
    ((mute solo heard) :rows +mute-solo-rows+)
  (let* ((inputs (list (mono 1/8 1/8) (mono 2/8 2/8) (mono 4/8 4/8)))
         (y (mix inputs :mute mute :solo solo :channels 1)))
    (ok (= 2 (pcm-buffer-frames y)))
    (ok (equalp (make-list 2 :initial-element (single (/ (reduce #'+ heard) 8)))
               (samples y)))))

(deftest generalized-booleans
  (ok (equalp '(0.25) (samples (mix (list (mono 1/8) (mono 1/4)) :solo '(nil :yes) :channels 1)))))

;;; Lengths --------------------------------------------------------------------------

(deftest the-longest-input-sets-the-length
  (let ((y (mix (list (mono 0.5) (mono 0.25 0.25 0.25)) :channels 1)))
    (ok (equalp '(0.75 0.25 0.25) (samples y))))
  (let ((y (mix (list (mono 0.5) (mono 0.25 0.25 0.25)) :channels 1 :mute '(nil t))))
    (ok (equalp '(0.5 0.0 0.0) (samples y)) "a muted input still counts"))
  (let ((y (mix (list (mono) (stereo)))))
    (ok (= 0 (pcm-buffer-frames y)))
    (ok (= 2 (pcm-buffer-channels y)))
    (ok (= 8000 (pcm-buffer-sample-rate y)))))

;;; Clipping -------------------------------------------------------------------------

(deftest clip-clamps-and-warns-once
  (multiple-value-bind (y warnings)
      (call-collecting-warnings
       (lambda () (mix (list (mono 0.75 -0.75 0.25 0.5) (mono 0.5 -0.5 0.25 0.5)) :channels 1)))
    (ok (equalp '(1.0 -1.0 0.5 1.0) (samples y)))
    (ok (= 1 (length warnings)))
    (ok (typep (first warnings) 'samples-clipped))
    (ok (and warnings (= 2 (samples-clipped-count (first warnings)))))
    (ok (and warnings (= 1.25 (samples-clipped-peak (first warnings)))))))

(deftest clip-is-the-default-and-one-is-not-clipped
  (multiple-value-bind (y warnings)
      (call-collecting-warnings (lambda () (mix (list (mono 0.5) (mono 0.5)) :channels 1)))
    (ok (equalp '(1.0) (samples y)))
    (ok (null warnings))))

(deftest scale-brings-the-peak-to-one
  (multiple-value-bind (y warnings)
      (call-collecting-warnings
       (lambda () (mix (list (mono 1.0 -0.5 0.25) (mono 1.0 -0.5 0.25)) :channels 1 :clip :scale)))
    (ok (equalp '(1.0 -0.5 0.25) (samples y)))
    (ok (null warnings)))
  (ok (equalp '(0.5 0.25) (samples (mix (list (mono 0.5 0.25)) :channels 1 :clip :scale)))
      "no change under the peak"))

(deftest none-keeps-overs
  (multiple-value-bind (y warnings)
      (call-collecting-warnings
       (lambda () (mix (list (mono 1.0 -1.0) (mono 0.5 -1.0)) :channels 1 :clip :none)))
    (ok (equalp '(1.5 -2.0) (samples y)))
    (ok (null warnings))))

;;; Mismatched inputs ----------------------------------------------------------------

(defun mismatch-of (thunk)
  "The AUDIO-MISMATCH-ERROR THUNK signals, or :NONE."
  (handler-case (progn (funcall thunk) :none)
    (audio-mismatch-error (c) c)))

(deftest another-rate-is-a-mismatch
  (let ((c (mismatch-of (lambda () (mix (list (mono 0.5) (buffer 16000 1 0.5)))))))
    (ok (typep c 'audio-mismatch-error))
    (ok (and (typep c 'audio-mismatch-error) (= 1 (audio-mismatch-error-index c)))))
  (let ((c (mismatch-of (lambda () (mix (list (mono 0.5) (mono 0.5) (buffer 8000 3 0 0 0)))))))
    (ok (and (typep c 'audio-mismatch-error) (= 2 (audio-mismatch-error-index c))))))

(deftest mismatch-restarts
  (ok (equalp '(audio-protocol:skip-input use-value)
             (restarts-offered (lambda () (mix (list (mono 0.5) (buffer 16000 1 0.5))))
                               'audio-mismatch-error '(audio-protocol:skip-input use-value)))))

(deftest skip-input-drops-its-settings-too
  (let ((y (invoking 'audio-mismatch-error 'audio-protocol:skip-input
                     (lambda () (mix (list (mono 0.5) (buffer 16000 1 0.5) (mono 0.25))
                                     :gains '(0 -6 0) :mute '(nil nil t) :channels 1)))))
    (ok (equalp '(0.5) (samples y)))))

(deftest skip-input-on-the-first-input
  (let ((y (invoking 'audio-mismatch-error 'audio-protocol:skip-input
                     (lambda () (mix (list (buffer 16000 3 0 0 0) (buffer 22050 1 0.5 0.5))
                                     :channels 1)))))
    (ok (= 22050 (pcm-buffer-sample-rate y)) "the first input left in sets the rate")
    (ok (equalp '(0.5 0.5) (samples y)))))

(deftest use-value-replaces-the-input
  (let ((y (invoking 'audio-mismatch-error 'use-value
                     (lambda () (mix (list (mono 0.5) (buffer 16000 1 0.5)) :channels 1))
                     (mono 0.125 0.125))))
    (ok (equalp '(0.625 0.125) (samples y))))
  (let* ((calls 0)
         (y (handler-bind ((audio-mismatch-error
                             (lambda (c)
                               (incf calls)
                               (use-value (if (= calls 1) (buffer 44100 1 0) (mono 0.25)) c))))
              (mix (list (mono 0.5) (buffer 16000 1 0.5)) :channels 1))))
    (ok (= 2 calls) "the value is checked again")
    (ok (equalp '(0.75) (samples y)))))

(deftest every-input-skipped
  (ok (signals (invoking 'audio-mismatch-error 'audio-protocol:skip-input
                         (lambda () (mix (list (buffer 8000 3 0 0 0)))))
               'audio-error)))

;;; Arguments ------------------------------------------------------------------------

(deftest mix-argument-errors
  (let ((a (mono 0.5)) (b (mono 0.25)))
    (ok (signals (mix '()) 'type-error))
    (ok (signals (mix (list a :b)) 'type-error))
    (ok (signals (mix (list a b) :gains '(0)) 'audio-error))
    (ok (signals (mix (list a b) :pans '(0 0 0)) 'audio-error))
    (ok (signals (mix (list a b) :mute '(t)) 'audio-error))
    (ok (signals (mix (list a b) :solo '(t t t)) 'audio-error))
    (ok (signals (mix (list a b) :gains '(0 :loud)) 'type-error))
    (ok (signals (mix (list a b) :pans '(0 2)) 'type-error))
    (ok (signals (mix (list a b) :clip :soft) 'type-error))
    (ok (signals (mix (list a b) :channels 3) 'type-error))
    (ok (signals (mix (list a b) :master "0") 'type-error))))

(deftest mix-leaves-its-inputs-alone
  (let* ((a (mono 0.75 -0.75)) (b (stereo 0.5 0.5 -0.5 -0.5))
         (a2 (mono 0.75 -0.75)) (b2 (stereo 0.5 0.5 -0.5 -0.5)))
    (mixed (list a b) :gains '(6 6) :clip :scale)
    (ok (equalp a2 a))
    (ok (equalp b2 b))))

(deftest mix-returns-a-fresh-buffer
  (let ((b (stereo 0.5 0.5)))
    (ok (not (eq (pcm-buffer-samples b) (pcm-buffer-samples (mix (list b))))))))
