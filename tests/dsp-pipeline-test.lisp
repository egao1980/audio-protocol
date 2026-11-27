;;;; AU3 pipeline: a melody rendered at 44.1 kHz by the reference engine,
;;;; downmixed, normalized, resampled to 16 kHz, cut into centred frames of
;;;; 2048 samples every 256, and each note's d' dips at its period. This is
;;;; the front end of a YIN or pYIN pitch tracker.

(in-package #:audio-protocol/tests)

(defparameter +melody+ '(57 60 64 67 72 69 65 62)
  "A3 C4 E4 G4 C5 A4 F4 D4: periods of 72.7 down to 30.6 samples at 16 kHz.")

(defun melody-smf ()
  "+MELODY+ at 120 quarters a minute: note i starts at 0.5 i s and lasts 0.375 s."
  (smf (track (named "Conductor") (tempo 0 500000))
       (track (named "Melody")
              (loop for key in +melody+ for i from 0
                    append (note (* 480 i) key 360)))))

(deftest pipeline-from-a-render-to-yin
  (let* ((rendered (render-midi (sine-engine) (melody-smf) :sample-rate 44100 :channels 2))
         (mono (dsp:downmix rendered))
         (loud (dsp:normalize mono :peak-db -1))
         (at-16k (resample loud 16000))
         (n (pcm-buffer-frames at-16k))
         (frames (dsp:frame-signal at-16k 2048 256))
         (levels (dsp:frame-rms at-16k 2048 256)))
    (ok (= 1 (pcm-buffer-channels mono)))
    (ok (approx= (db-to-gain -1) (dsp:peak loud) 1d-6) "normalized to -1 dBFS")
    (ok (= 16000 (pcm-buffer-sample-rate at-16k)))
    (ok (= (ceiling n 256) (length frames) (dsp:frame-count n 2048 256))
        "one centred frame every 256 samples")
    (ok (= (length frames) (length levels)))
    (loop for key in +melody+
          for i from 0
          for index = (round (+ (* 8000 i) 3000) 256) ; the frame centred mid-note
          for d-prime = (dsp:yin-cmndf (aref frames index))
          for period = (/ 16000 (note-frequency key))
          for tau = (first-dip d-prime 16)
          do (ok (and tau (<= (abs (- tau period)) 1))
                 (format nil "key ~D: period ~,2F samples, first dip at ~A" key period tau))
             (ok (and tau (< (aref d-prime tau) 0.1d0)) "a deep dip")
             (ok (approx= (/ (db-to-gain -1) (sqrt 2d0)) (aref levels index) 1d-2)
                 "the frame's RMS is the tone's"))))
