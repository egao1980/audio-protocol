;;;; AU0 engines: durations and frames, the protocol's :AROUND checks, and the
;;;; reference engine (a C major scale: onsets, frequencies, exact values).

(in-package #:audio-protocol/tests)

;;; Building SMFs -------------------------------------------------------------------

(defun track (&rest timed)
  "A track from (tick . event) conses and lists of them (NOTE gives two)."
  (stack-midi:track-from-absolute
   (loop for x in timed
         if (and (consp x) (integerp (car x))) collect x
         else append x)))

(defun note (tick key duration &key (channel 0) (velocity 100))
  "Note-on at TICK and note-off at TICK + DURATION, as (tick . event) conses."
  (list (cons tick (stack-midi:make-note-on-event :channel channel :key key :velocity velocity))
        (cons (+ tick duration) (stack-midi:make-note-off-event :channel channel :key key))))

(defun tempo (tick microseconds)
  (cons tick (stack-midi:make-tempo-event :tempo microseconds)))

(defun named (name) (cons 0 (stack-midi:make-track-name-event :text name)))

(defun smf (&rest tracks)
  (stack-midi:make-smf :format 1 :division 480 :tracks tracks))

(defparameter +c-major+ '(60 62 64 65 67 69 71 72) "C4 .. C5.")

(defun scale-smf (&key (velocity 100))
  "The C major scale at 120 quarters a minute, 480 ticks a quarter: note i
starts at 480 i ticks (0.5 i s) and lasts 360 ticks (0.375 s)."
  (smf (track (named "Conductor") (tempo 0 500000))
       (track (named "Scale")
              (loop for key in +c-major+ for i from 0
                    append (note (* 480 i) key 360 :velocity velocity)))))

(defun square-engine (&optional (amplitude 1/4))
  (make-instance 'reference-engine :waveform :square :amplitude amplitude))

(defun sine-engine (&optional (amplitude 1/4))
  (make-instance 'reference-engine :waveform :sine :amplitude amplitude))

(defun tone-value (waveform amplitude velocity key m rate)
  "A reference-engine note's value, straight from engine.lisp."
  (let ((phi (/ (* (note-frequency key) m) rate)))
    (* (float amplitude 1d0) (/ velocity 127d0)
       (ecase waveform
         (:sine (sin (* 2 (dpi) phi)))
         (:square (if (< (- phi (floor phi)) 1/2) 1d0 -1d0))))))

(defun expected-render (notes frames rate &key (waveform :sine) (amplitude 1/4))
  "Channel 0 of a reference render: NOTES are (n0 n1 key velocity)."
  (let ((v (make-array frames :element-type 'double-float :initial-element 0d0)))
    (loop for (n0 n1 key velocity) in notes
          do (loop for n from n0 below (min n1 frames)
                   do (incf (aref v n) (tone-value waveform amplitude velocity key (- n n0) rate))))
    v))

(defun matches-render (expected buffer &optional (tolerance 1d-6))
  "Every channel of BUFFER is within TOLERANCE of EXPECTED: list of bad frames."
  (loop for n below (length expected)
        unless (loop for c below (pcm-buffer-channels buffer)
                     always (approx= (aref expected n) (sample-ref buffer n c) tolerance))
          collect n))

;;; Duration and frames -------------------------------------------------------------

(deftest smf-durations
  (ok (= 31/8 (smf-duration (scale-smf))) "3720 ticks at 120 quarters a minute")
  (ok (= 2 (smf-duration (smf (track (tempo 0 500000) (tempo 960 1000000)
                                     (cons 1440 (stack-midi:make-end-of-track-event)))))))
  (ok (= 0 (smf-duration (stack-midi:make-smf :format 1 :division 96 :tracks '()))))
  (ok (= 5/2 (smf-duration (stack-midi:make-smf
                            :format 0
                            :division (stack-midi:make-smpte-division :frames-per-second 25
                                                                      :ticks-per-frame 40)
                            :tracks (list (track (note 0 60 2500))))))))

(deftest render-frame-counts
  (ok (= 39000 (render-frames (scale-smf) 8000)) "tail 1 s by default")
  (ok (= 31000 (render-frames (scale-smf) 8000 0)))
  (ok (= 35000 (render-frames (scale-smf) 8000 1/2)))
  (ok (= 33000 (render-frames (scale-smf) 8000 0.25d0)))
  (ok (= 214988 (render-frames (scale-smf) 44100))))

;;; The reference engine ------------------------------------------------------------

(defun note-onsets (vector)
  "Frames where VECTOR turns non-zero after a zero (or at 0)."
  (loop for n below (length vector)
        when (and (/= 0 (aref vector n)) (or (zerop n) (zerop (aref vector (1- n)))))
          collect n))

(defun note-ends (vector)
  "Frames after which VECTOR turns zero: the last frame of each sounding run."
  (loop for n below (length vector)
        when (and (/= 0 (aref vector n))
                  (or (= n (1- (length vector))) (zerop (aref vector (1+ n)))))
          collect n))

(deftest c-major-onsets-and-ends
  (let* ((b (render-midi (square-engine) (scale-smf) :sample-rate 8000 :channels 1))
         (v (pcm-channel b 0)))
    (ok (= 39000 (pcm-buffer-frames b)))
    (ok (equal (loop for i below 8 collect (* 4000 i)) (note-onsets v)))
    (ok (equal (loop for i below 8 collect (+ (* 4000 i) 2999)) (note-ends v)))
    (ok (approx= (* 1/4 100/127) (aref v 0) 1d-7) "a square note starts at full value")
    (ok (every #'zerop (subseq v 31000)) "the tail is silent")))

(deftest c-major-frequencies
  (let* ((b (render-midi (sine-engine) (scale-smf) :sample-rate 8000 :channels 1))
         (v (pcm-channel b 0)))
    (loop for key in +c-major+ for i from 0
          do (let* ((f (note-frequency key))
                    (start (* 4000 i))
                    (end (+ start 3000)))
               (ok (approx= f (zero-crossing-frequency v start end 8000) (* 0.01 f))
                   (format nil "key ~D by zero crossings" key))
               (let ((here (goertzel-power v start end 8000 f))
                     (below (goertzel-power v start end 8000 (/ f (expt 2 1/12))))
                     (above (goertzel-power v start end 8000 (* f (expt 2 1/12)))))
                 (ok (and (> here (* 50 below)) (> here (* 50 above)))
                     (format nil "key ~D by Goertzel against its neighbours" key)))))))

(deftest c-major-exact-values
  (dolist (waveform '(:sine :square))
    (let* ((b (render-midi (make-instance 'reference-engine :waveform waveform)
                           (scale-smf) :sample-rate 8000))
           (want (expected-render (loop for key in +c-major+ for i from 0
                                        collect (list (* 4000 i) (+ (* 4000 i) 3000) key 100))
                                  39000 8000 :waveform waveform)))
      (ok (= 2 (pcm-buffer-channels b)))
      (ok (null (matches-render want b)) (format nil "~A: every frame, both channels" waveform)))))

(deftest-parametrize velocity-and-amplitude
    ((velocity amplitude) :rows '((127 1/4) (64 1/4) (1 1/4) (100 1) (100 1/10)))
  (let ((b (render-midi (square-engine amplitude) (smf (track (note 0 69 480 :velocity velocity)))
                        :sample-rate 8000 :channels 1)))
    (ok (approx= (* amplitude (/ velocity 127)) (sample-ref b 0 0) 1d-7))))

(deftest-parametrize note-matching
    ((label events notes)
     :rows
     (list
      (list "first in, first out"
            (list (note 0 69 960) (note 480 69 960))
            '((0 8000 69 100) (4000 12000 69 100)))
      (list "velocity 0 ends a note"
            (list (cons 0 (stack-midi:make-note-on-event :key 64 :velocity 90))
                  (cons 240 (stack-midi:make-note-on-event :key 64 :velocity 0)))
            '((0 2000 64 90)))
      (list "another channel's note-off doesn't end it"
            (list (cons 0 (stack-midi:make-note-on-event :channel 2 :key 64 :velocity 90))
                  (cons 240 (stack-midi:make-note-off-event :channel 3 :key 64))
                  (cons 480 (stack-midi:make-note-off-event :channel 2 :key 64)))
            '((0 4000 64 90)))
      (list "a stray note-off is ignored"
            (list (cons 0 (stack-midi:make-note-off-event :key 60))
                  (note 120 60 120))
            '((1000 2000 60 100)))
      (list "never ended: stops at the end of the SMF"
            (list (cons 0 (stack-midi:make-note-on-event :key 72 :velocity 50))
                  (cons 960 (stack-midi:make-end-of-track-event)))
            '((0 8000 72 50)))
      (list "re-struck at the same tick: off first, then on"
            (list (cons 0 (stack-midi:make-note-on-event :key 67 :velocity 80))
                  (cons 480 (stack-midi:make-note-off-event :key 67))
                  (cons 480 (stack-midi:make-note-on-event :key 67 :velocity 40))
                  (cons 960 (stack-midi:make-note-off-event :key 67)))
            '((0 4000 67 80) (4000 8000 67 40)))
      (list "zero length: on then off at one tick"
            (list (cons 240 (stack-midi:make-note-on-event :key 67 :velocity 80))
                  (cons 240 (stack-midi:make-note-off-event :key 67))
                  (cons 480 (stack-midi:make-end-of-track-event)))
            '())
      (list "a chord sums"
            (list (note 0 60 480) (note 0 64 480 :velocity 70) (note 0 67 480 :velocity 50))
            '((0 4000 60 100) (0 4000 64 70) (0 4000 67 50)))
      (list "other events are ignored"
            (list (cons 0 (stack-midi:make-program-change-event :program 40))
                  (cons 0 (stack-midi:make-control-change-event :controller 7 :value 10))
                  (cons 0 (stack-midi:make-pitch-bend-event :value 8191))
                  (note 0 60 480 :channel 9))
            '((0 4000 60 100)))))
  (declare (ignorable label))
  (dolist (waveform '(:sine :square))
    (let* ((s (smf (track (tempo 0 500000)) (apply #'track events)))
           (b (render-midi (make-instance 'reference-engine :waveform waveform) s
                           :sample-rate 8000 :tail 1/4))
           (frames (render-frames s 8000 1/4)))
      (ok (= frames (pcm-buffer-frames b)))
      (ok (null (matches-render (expected-render notes frames 8000 :waveform waveform) b))
          (format nil "~A ~A" label waveform)))))

(deftest notes-in-several-tracks-merge
  (let ((b (render-midi (sine-engine)
                        (smf (track (tempo 0 500000)) (track (note 0 60 480))
                             (track (note 240 67 480 :channel 1)))
                        :sample-rate 8000 :tail 0)))
    (ok (null (matches-render (expected-render '((0 4000 60 100) (2000 6000 67 100)) 6000 8000)
                              b)))))

(deftest tempo-changes-move-notes
  ;; 120 quarters a minute, then 60 from tick 960: a note at 1440 is at 2 s.
  (let ((b (render-midi (square-engine)
                        (smf (track (tempo 0 500000) (tempo 960 1000000))
                             (track (note 0 60 240) (note 1440 62 240)))
                        :sample-rate 8000 :channels 1)))
    (ok (equal '(0 16000) (note-onsets (pcm-channel b 0))))
    (ok (equal '(1999 19999) (note-ends (pcm-channel b 0))))))

(deftest other-sample-rates
  (let ((b (render-midi (square-engine) (scale-smf) :sample-rate 44100 :channels 1)))
    (ok (= 214988 (pcm-buffer-frames b)))
    (ok (equal (loop for i below 8 collect (seconds-to-frame (/ i 2) 44100))
               (note-onsets (pcm-channel b 0))))))

(deftest smf-designators
  (let* ((s (scale-smf))
         (want (render-midi (sine-engine) s :sample-rate 8000)))
    (ok (equalp want (render-midi (sine-engine) (stack-midi:write-smf s nil) :sample-rate 8000))
        "octets")
    (with-temp-dir (dir)
      (let ((path (merge-pathnames "scale.mid" dir)))
        (stack-midi:write-smf s path)
        (ok (equalp want (render-midi (sine-engine) path :sample-rate 8000)) "pathname")))))

(deftest soundfont-is-ignored-by-the-reference
  (ok (equalp (render-midi (sine-engine) (scale-smf) :sample-rate 8000)
              (render-midi (sine-engine) (scale-smf) :sample-rate 8000
                                                     :soundfont #p"/no/such/bank.sf2"))))

(deftest render-argument-errors
  (let ((s (scale-smf)) (e (sine-engine)))
    (ok (signals (render-midi e s :sample-rate 0) 'type-error))
    (ok (signals (render-midi e s :sample-rate 8000.0) 'type-error))
    (ok (signals (render-midi e s :channels 3) 'type-error))
    (ok (signals (render-midi e s :tail -1) 'type-error))
    (ok (signals (render-midi e s :soundfont 42) 'type-error))
    (ok (signals (render-midi e :not-an-smf) 'type-error))
    (ok (signals (render-stems e s :by :voice) 'type-error))))

(deftest render-reads-bad-bytes-through-midi-protocol
  (ok (signals (render-midi (sine-engine) (octets "MThd" 0 0 0 6 0 7)) 'stack-midi:midi-parse-error)))

;;; The protocol's :AROUND checks ---------------------------------------------------

(defclass bare-engine (audio-engine) ())

(defclass recording-engine (audio-engine)
  ((calls :initform '() :accessor engine-calls)
   (frames-offset :initarg :frames-offset :initform 0 :reader frames-offset)
   (result-channels :initarg :result-channels :initform nil :reader result-channels)))

(defmethod render-midi ((engine recording-engine) smf &rest keys
                        &key sample-rate soundfont channels tail)
  (declare (ignore soundfont))
  (push (list smf keys *max-render-seconds*) (engine-calls engine))
  (make-pcm-buffer :sample-rate sample-rate
                   :channels (or (result-channels engine) channels)
                   :frames (+ (frames-offset engine) (render-frames smf sample-rate tail))))

(defclass listing-engine (audio-engine) ())

(defmethod render-midi ((engine listing-engine) smf &key &allow-other-keys)
  (declare (ignore smf))
  (list :not-a-buffer))

(deftest the-base-class-is-unavailable
  (let* ((engine (make-instance 'bare-engine))
         (c (handler-case (progn (render-midi engine (scale-smf)) :none)
              (audio-backend-unavailable (c) c))))
    (ok (typep c 'audio-backend-unavailable))
    (ok (and (typep c 'audio-engine-error) (eq engine (audio-engine-error-engine c))))))

(deftest methods-get-an-smf-and-every-keyword
  (let ((engine (make-instance 'recording-engine)))
    (render-midi engine (stack-midi:write-smf (scale-smf) nil))
    (destructuring-bind (smf keys limit) (first (engine-calls engine))
      (declare (ignore limit))
      (ok (stack-midi:smf-p smf) "a designator arrives as an SMF")
      (ok (equalp (scale-smf) smf))
      (ok (eql 44100 (getf keys :sample-rate)))
      (ok (eql 2 (getf keys :channels)))
      (ok (eql 1 (getf keys :tail)))
      (ok (member :soundfont keys) "SOUNDFONT given explicitly (NIL)"))))

(deftest results-are-checked
  (dolist (engine (list (make-instance 'recording-engine :frames-offset 1)
                        (make-instance 'recording-engine :frames-offset -1)
                        (make-instance 'recording-engine :result-channels 1)
                        (make-instance 'listing-engine)))
    (let ((c (handler-case (progn (render-midi engine (scale-smf) :sample-rate 8000) :none)
               (audio-engine-error (c) c))))
      (ok (typep c 'audio-engine-error))
      (ok (and (typep c 'audio-engine-error) (eq engine (audio-engine-error-engine c))))))
  (ok (pcm-buffer-p (render-midi (make-instance 'recording-engine) (scale-smf) :sample-rate 8000))))

(deftest long-renders-ask-first
  (let ((*max-render-seconds* 4))
    (let ((c (handler-case (progn (render-midi (sine-engine) (scale-smf) :sample-rate 8000) :none)
               (audio-limit-exceeded (c) c))))
      (ok (typep c 'audio-limit-exceeded))
      (ok (and (typep c 'audio-limit-exceeded) (= 4 (audio-limit-exceeded-limit c))))
      (ok (and (typep c 'audio-limit-exceeded) (= 39/8 (audio-limit-exceeded-value c)))))
    (ok (equal '(continue)
               (restarts-offered (lambda () (render-midi (sine-engine) (scale-smf) :sample-rate 8000))
                                 'audio-limit-exceeded '(continue))))
    (ok (= 39000 (pcm-buffer-frames
                  (invoking 'audio-limit-exceeded 'continue
                            (lambda () (render-midi (sine-engine) (scale-smf) :sample-rate 8000)))))
        "CONTINUE renders anyway")
    (ok (pcm-buffer-p (render-midi (sine-engine) (scale-smf) :sample-rate 8000 :tail 0))
        "3.875 s fits"))
  (let ((*max-render-seconds* nil))
    (ok (pcm-buffer-p (render-midi (sine-engine) (scale-smf) :sample-rate 1 :tail 100000))
        "NIL: no limit")))
