;;;; AU0 RENDER-STEMS: one stem per track or channel, the filtered SMFs, equal
;;;; lengths, stems summing to the mixdown, and rehearsal mixes through MIX.

(in-package #:audio-protocol/tests)

(defun choir-smf ()
  "Conductor; Soprano on channel 0; Alto on channel 1; an unnamed track on
channel 2; Pedal (controllers only, channel 3); Doubling on channel 0 again."
  (smf (track (named "Conductor") (tempo 0 500000) (tempo 960 400000))
       (track (named "Soprano") (note 0 72 480) (note 480 74 480) (note 960 76 960))
       (track (named "Alto") (note 0 64 960 :channel 1) (note 960 65 480 :channel 1))
       (track (cons 0 (stack-midi:make-program-change-event :channel 2 :program 32))
              (note 0 48 1920 :channel 2 :velocity 60))
       (track (named "Pedal")
              (cons 0 (stack-midi:make-control-change-event :channel 3 :controller 64 :value 127))
              (cons 960 (stack-midi:make-control-change-event :channel 3 :controller 64 :value 0)))
       (track (named "Doubling") (note 1440 60 480 :velocity 30))))

(defun keep-channel-events (smf keep-p)
  "SMF with every non-channel event, and the channel events (track-index event)
satisfies KEEP-P: the filtered SMF of engine.lisp, built here independently."
  (stack-midi:make-smf
   :format (stack-midi:smf-format smf) :division (stack-midi:smf-division smf)
   :tracks (loop for track in (stack-midi:smf-tracks smf)
                 for i from 0
                 collect (stack-midi:track-from-absolute
                          (remove-if (lambda (timed)
                                       (and (stack-midi:channel-event-p (cdr timed))
                                            (not (funcall keep-p i (cdr timed)))))
                                     (stack-midi:track-to-absolute track))))))

(defun stem-summary (stems)
  (mapcar (lambda (s) (list (stem-name s) (stem-track s) (stem-channel s))) stems))

(deftest stems-by-track
  (let ((stems (render-stems (sine-engine) (choir-smf) :sample-rate 8000)))
    (ok (equal '(("Soprano" 1 nil) ("Alto" 2 nil) ("Track 3" 3 nil) ("Doubling" 5 nil))
               (stem-summary stems)))))

(deftest stems-by-channel
  (let ((stems (render-stems (sine-engine) (choir-smf) :sample-rate 8000 :by :channel)))
    (ok (equal '(("Channel 1" nil 0) ("Channel 2" nil 1) ("Channel 3" nil 2))
               (stem-summary stems)))))

(deftest-parametrize every-stem-has-the-render-shape
    ((by channels tail) :rows '((:track 2 1) (:track 1 0) (:channel 2 1/2) (:channel 1 3)))
  (let* ((s (choir-smf))
         (stems (render-stems (sine-engine) s :sample-rate 8000 :by by :channels channels :tail tail))
         (frames (render-frames s 8000 tail)))
    (ok (every (lambda (stem)
                 (let ((b (stem-buffer stem)))
                   (and (= 8000 (pcm-buffer-sample-rate b))
                        (= channels (pcm-buffer-channels b))
                        (= frames (pcm-buffer-frames b)))))
               stems))))

(deftest-parametrize a-stem-is-the-render-of-its-filtered-smf
    ((by index keep)
     :rows (list (list :track 0 (lambda (i e) (declare (ignore e)) (= i 1)))
                 (list :track 2 (lambda (i e) (declare (ignore e)) (= i 3)))
                 (list :channel 0 (lambda (i e) (declare (ignore i))
                                    (= 0 (stack-midi:channel-event-channel e))))
                 (list :channel 1 (lambda (i e) (declare (ignore i))
                                    (= 1 (stack-midi:channel-event-channel e))))))
  (let* ((s (choir-smf))
         (stem (nth index (render-stems (sine-engine) s :sample-rate 8000 :by by))))
    (ok (equalp (render-midi (sine-engine) (keep-channel-events s keep) :sample-rate 8000)
                (stem-buffer stem)))))

(deftest-parametrize stems-sum-to-the-mixdown
    ((by) :rows '(:track :channel))
  (let* ((s (choir-smf))
         (mixdown (render-midi (sine-engine) s :sample-rate 8000))
         (stems (render-stems (sine-engine) s :sample-rate 8000 :by by))
         (sum (mix stems :clip :none)))
    (ok (every-approx= (samples mixdown) (samples sum) 1d-6))))

(deftest a-rehearsal-mix-brings-one-stem-forward
  (let* ((stems (render-stems (sine-engine) (choir-smf) :sample-rate 8000))
         (alto (pcm-channel (stem-buffer (second stems)) 0))
         (y (mix stems :gains '(-18 0 -18 -18) :clip :none))
         (left (pcm-channel y 0)))
    ;; Alto alone sounds from 0 to 1440 ticks; the mix there is mostly Alto.
    (ok (> (goertzel-power left 0 8000 8000 (note-frequency 64))
           (* 10 (goertzel-power left 0 8000 8000 (note-frequency 72)))))
    (ok (approx= (rms alto 0 8000) (rms left 0 8000) (* 0.05 (rms alto 0 8000)))
        "stereo stems at pan 0 keep their level")))

(deftest no-notes-no-stems
  (ok (null (render-stems (sine-engine) (smf (track (tempo 0 500000))
                                             (track (cons 0 (stack-midi:make-note-on-event
                                                             :key 60 :velocity 0))))
                          :sample-rate 8000)))
  (ok (null (render-stems (sine-engine) (smf) :sample-rate 8000 :by :channel))))

(deftest track-names-come-from-the-first-track-name-event
  (let ((stems (render-stems (sine-engine)
                             (smf (track (tempo 0 500000))
                                  (track (named "First") (cons 10 (stack-midi:make-track-name-event
                                                                   :text "Second"))
                                         (note 0 60 480)))
                             :sample-rate 8000)))
    (ok (equal '("First") (mapcar #'stem-name stems)))))

(deftest stems-through-the-default-method
  (let* ((engine (make-instance 'recording-engine))
         (s (choir-smf))
         (stems (render-stems engine s :sample-rate 8000 :tail 1/2))
         (calls (reverse (engine-calls engine))))
    (ok (= 4 (length stems)))
    (ok (= 4 (length calls)) "one RENDER-MIDI per stem")
    (loop for (smf keys limit) in calls
          for index in '(1 2 3 5)
          do (ok (null limit) "inner renders run with *MAX-RENDER-SECONDS* NIL")
             (ok (= (smf-duration s) (smf-duration smf)) "the duration doesn't move")
             (ok (equal (stack-midi:tempo-map s) (stack-midi:tempo-map smf)) "nor the tempo map")
             (ok (equalp (keep-channel-events s (lambda (i e) (declare (ignore e)) (= i index))) smf))
             (ok (eql 8000 (getf keys :sample-rate)))
             (ok (eql 1/2 (getf keys :tail))))))

(deftest stem-results-are-checked
  (let ((c (handler-case (progn (render-stems (make-instance 'recording-engine :frames-offset 2)
                                              (choir-smf) :sample-rate 8000)
                                :none)
             (audio-engine-error (c) c))))
    (ok (typep c 'audio-engine-error))))

(defclass one-pass-engine (audio-engine) ())

(defmethod render-midi ((engine one-pass-engine) smf &key sample-rate channels tail
                        &allow-other-keys)
  (make-pcm-buffer :sample-rate sample-rate :channels channels
                   :frames (render-frames smf sample-rate tail)))

(defmethod render-stems ((engine one-pass-engine) smf &key sample-rate channels tail
                         &allow-other-keys)
  (list (make-stem :name "all" :buffer (make-pcm-buffer :sample-rate sample-rate :channels channels
                                                        :frames (render-frames smf sample-rate tail)))
        (make-stem :name "short" :buffer (make-pcm-buffer :sample-rate sample-rate
                                                          :channels channels :frames 1))))

(deftest a-backend-render-stems-is-checked-too
  (ok (signals (render-stems (make-instance 'one-pass-engine) (choir-smf) :sample-rate 8000)
               'audio-engine-error)))

(deftest long-stem-renders-ask-once
  (let ((*max-render-seconds* 1)
        (asked 0))
    (let ((stems (handler-bind ((audio-limit-exceeded
                                  (lambda (c) (incf asked) (continue c))))
                   (render-stems (sine-engine) (choir-smf) :sample-rate 8000))))
      (ok (= 1 asked))
      (ok (= 4 (length stems))))))
