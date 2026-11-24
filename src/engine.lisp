;;;; AU0 engines: the protocol for rendering Standard MIDI Files to PCM, and a
;;;; reference engine.
;;;;
;;;; SMFs are midi-protocol's (STACK-MIDI:SMF and its events); times come from
;;;; STACK-MIDI:TEMPO-MAP and STACK-MIDI:TICKS-TO-SECONDS, exact rationals, and become
;;;; frames once, through SECONDS-TO-FRAME.
;;;;
;;;; (SMF-DURATION smf) -> seconds, an exact rational: the time of the latest
;;;;   tick of SMF, (stack-midi:ticks-to-seconds T division (stack-midi:tempo-map smf)),
;;;;   where T is the largest sum of a track's deltas (its END-OF-TRACK-EVENT
;;;;   included), 0 for an SMF with no tracks.
;;;; (RENDER-FRAMES smf sample-rate &optional (tail 1)) -> the frame count of
;;;;   every render: (seconds-to-frame (+ (smf-duration smf) tail) sample-rate).
;;;;   TAIL, a non-negative real in seconds, is room for releases and reverb.
;;;;
;;;; Generic functions. Methods specialise ENGINE only: the protocol's :AROUND
;;;; methods below replace the SMF argument (a designator becomes an SMF), and
;;;; CALL-NEXT-METHOD with new arguments must not change the set of applicable
;;;; methods (CLHS CALL-NEXT-METHOD).
;;;;
;;;; (RENDER-MIDI engine smf &key (sample-rate 44100) soundfont (channels 2) (tail 1))
;;;;   -> a PCM-BUFFER: the whole SMF, every track, mixed by the engine.
;;;; (RENDER-STEMS engine smf &key (sample-rate 44100) soundfont (channels 2) (tail 1)
;;;;               (by :track))
;;;;   -> a fresh list of STEMs, one per track (BY :TRACK) or MIDI channel
;;;;   (BY :CHANNEL) that plays notes, for rehearsal mixes (MIX).
;;;;
;;;; The :AROUND method on AUDIO-ENGINE of each, in this order:
;;;;   1. SMF is a STACK-MIDI:SMF, or anything STACK-MIDI:READ-SMF reads (an octet vector,
;;;;      a binary input stream, a pathname designator), which it reads; its
;;;;      conditions and restarts reach the caller.
;;;;   2. Arguments, else TYPE-ERROR: SAMPLE-RATE an integer 1..#xFFFFFFFF;
;;;;      SOUNDFONT NIL or a pathname designator (engines that need one signal
;;;;      AUDIO-BACKEND-UNAVAILABLE when it is NIL or missing); CHANNELS 1 or 2;
;;;;      TAIL a non-negative real; BY :TRACK or :CHANNEL.
;;;;   3. When *MAX-RENDER-SECONDS* is not NIL and (+ (smf-duration smf) tail)
;;;;      exceeds it: AUDIO-LIMIT-EXCEEDED (LIMIT *MAX-RENDER-SECONDS*, VALUE
;;;;      that sum) with ERROR inside a CONTINUE restart, which renders anyway.
;;;;   4. CALL-NEXT-METHOD with the SMF and every keyword argument given
;;;;      explicitly (the defaults filled in).
;;;;   5. Checks the result. RENDER-MIDI: a PCM-BUFFER with SAMPLE-RATE,
;;;;      CHANNELS and (render-frames smf sample-rate tail) frames. RENDER-STEMS:
;;;;      a list of STEMs whose buffers are all such. Anything else signals
;;;;      AUDIO-ENGINE-ERROR (ENGINE the engine). Returns the result.
;;;; So a backend writes primary methods only, gets SMFs and checked arguments,
;;;; and every render of one SMF at one rate and tail has one length.
;;;;
;;;; The primary methods on AUDIO-ENGINE:
;;;;   RENDER-MIDI signals AUDIO-BACKEND-UNAVAILABLE (ENGINE, REASON "no
;;;;   render-midi method"): an engine class must define one.
;;;;   RENDER-STEMS renders each stem with RENDER-MIDI on a filtered SMF (a
;;;;   backend may override it to render every stem in one pass, as long as
;;;;   the stems are the same):
;;;;     BY :TRACK   one stem per track holding at least one NOTE-ON-EVENT with
;;;;                 velocity > 0, in track order. NAME is the TEXT of the
;;;;                 track's first TRACK-NAME-EVENT, else "Track i" for the
;;;;                 0-based track index i; TRACK is i; CHANNEL NIL. Its SMF keeps
;;;;                 every track, every non-channel event (meta, sysex, escape)
;;;;                 of every track, and the CHANNEL-EVENTs of track i only.
;;;;     BY :CHANNEL one stem per MIDI channel c (0..15) that has a NOTE-ON-EVENT
;;;;                 with velocity > 0 in some track, ascending. NAME is
;;;;                 "Channel c+1" (counted from 1, as players do); CHANNEL is c;
;;;;                 TRACK NIL. Its SMF keeps every non-channel event and the
;;;;                 CHANNEL-EVENTs of channel c only.
;;;;   Each filtered track is rebuilt with STACK-MIDI:TRACK-TO-ABSOLUTE and
;;;;   STACK-MIDI:TRACK-FROM-ABSOLUTE, so absolute times, the tempo map and the SMF's
;;;;   duration don't move; FORMAT and DIVISION are kept. The RENDER-MIDI calls
;;;;   get the same SAMPLE-RATE, SOUNDFONT, CHANNELS and TAIL, with
;;;;   *MAX-RENDER-SECONDS* bound to NIL (step 3 ran once already). An SMF with
;;;;   no notes gives NIL.
;;;;
;;;; REFERENCE-ENGINE renders tones, so the protocol and its consumers can be
;;;; tested without a SoundFont synthesiser. Its RENDER-MIDI:
;;;;   Order. Every event of every track at its absolute tick, in MERGE-TRACKS
;;;;     order (by tick, then track, then place in the track). A tick t is at
;;;;     frame (seconds-to-frame (stack-midi:ticks-to-seconds t division tempo-map)
;;;;     sample-rate), the tempo map of the whole SMF (also for format 2).
;;;;   Notes. A NOTE-ON-EVENT with velocity v > 0, channel c, key k, at frame n0
;;;;     starts a note. It ends at the frame n1 of the first NOTE-OFF-EVENT, or
;;;;     NOTE-ON-EVENT with velocity 0, of channel c and key k that comes after
;;;;     it in that order and doesn't already end an earlier note (notes of one
;;;;     channel and key end first in, first out). A note never ended stops at
;;;;     (seconds-to-frame (smf-duration smf) sample-rate). It sounds on the
;;;;     frames n0 <= n < n1 (none when n1 <= n0). Every other event is ignored:
;;;;     programs, controllers, pitch bend and channel 10 all play tones.
;;;;   Tone. At frame n, with m = n - n0, f = (note-frequency k) and
;;;;     phi = f * m / sample-rate, a note's value is (A * v / 127) * W(phi),
;;;;     A the engine's AMPLITUDE, W(phi) = sin(2 pi phi) for WAVEFORM :SINE, and
;;;;     for :SQUARE 1 when phi - floor(phi) < 1/2, else -1. No envelope: a
;;;;     square note starts at full value, a sine note at 0.
;;;;   Output. Every channel of frame n holds the sum of the values of the notes
;;;;     sounding at n, rounded to a SINGLE-FLOAT. Computed in DOUBLE-FLOAT. No
;;;;     clipping (MIX has the policies). SOUNDFONT is ignored.
;;;;   Results stay within 1e-6 of these formulas.

(in-package #:audio-protocol)

(defvar *max-render-seconds* 3600
  "Longest render (SMF duration plus tail, in seconds) RENDER-MIDI and
RENDER-STEMS start without asking (AUDIO-LIMIT-EXCEEDED, CONTINUE). NIL: no limit.")

(defclass audio-engine ()
  ()
  (:documentation "Renders Standard MIDI Files to PCM. Backends subclass it and
define RENDER-MIDI (and may define RENDER-STEMS) methods on their class."))

(defgeneric render-midi (engine smf &key sample-rate soundfont channels tail)
  (:documentation "Render SMF (a STACK-MIDI:SMF or a READ-SMF source) to one
PCM-BUFFER of CHANNELS at SAMPLE-RATE, (render-frames smf sample-rate tail)
frames long. See engine.lisp."))

(defgeneric render-stems (engine smf &key sample-rate soundfont channels tail by)
  (:documentation "Render SMF to a list of STEMs, one per track (BY :TRACK) or
channel (BY :CHANNEL) that plays notes, every buffer as RENDER-MIDI's. See
engine.lisp."))

(defclass reference-engine (audio-engine)
  ((waveform :initarg :waveform :initform :sine :type (member :sine :square)
             :reader reference-engine-waveform
             :documentation ":SINE or :SQUARE.")
   (amplitude :initarg :amplitude :initform 1/4 :type (real (0) 1)
              :reader reference-engine-amplitude
              :documentation "Value of a note at velocity 127, a real in (0, 1]."))
  (:documentation "A tiny tone synth: one sine or square tone per note, no
envelope, no SoundFont. See engine.lisp."))

(defun smf-duration (smf)
  "Exact seconds from the start of SMF to its latest tick."
  (check-type smf stack-midi:smf)
  (stack-midi:ticks-to-seconds
   (reduce #'max (stack-midi:smf-tracks smf)
           :key (lambda (track) (reduce #'+ track :key #'stack-midi:midi-event-delta))
           :initial-value 0)
   (stack-midi:smf-division smf)
   (stack-midi:tempo-map smf)))

(defun render-frames (smf sample-rate &optional (tail 1))
  "Frames of every render of SMF at SAMPLE-RATE with TAIL seconds after its end."
  (check-type tail (real 0))
  (seconds-to-frame (+ (smf-duration smf) tail) sample-rate))

(defun %designated-smf (smf)
  "SMF itself, or the SMF STACK-MIDI:READ-SMF reads from it: its conditions and
restarts reach the caller, and a non-designator is its TYPE-ERROR."
  (if (stack-midi:smf-p smf) smf (stack-midi:read-smf smf)))

(defun %check-render-limit (smf tail)
  "When SMF's duration plus TAIL exceeds *MAX-RENDER-SECONDS* (not NIL), signal
AUDIO-LIMIT-EXCEEDED inside a CONTINUE restart, which renders anyway."
  (let ((limit *max-render-seconds*)
        (seconds (+ (smf-duration smf) tail)))
    (when (and limit (> seconds limit))
      (restart-case (error 'audio-limit-exceeded
                           :limit limit :value seconds
                           :reason "a render longer than *MAX-RENDER-SECONDS*")
        (continue ()
          :report "Render anyway."
          nil)))))

(defun %render-shape-p (buffer smf sample-rate channels tail)
  "Is BUFFER a PCM-BUFFER of SAMPLE-RATE, CHANNELS and (render-frames smf
sample-rate tail) frames?"
  (and (pcm-buffer-p buffer)
       (= sample-rate (pcm-buffer-sample-rate buffer))
       (= channels (pcm-buffer-channels buffer))
       (= (render-frames smf sample-rate tail) (pcm-buffer-frames buffer))))

(defun %note-p (event)
  "Does EVENT start a note: a NOTE-ON-EVENT with velocity > 0?"
  (and (stack-midi:note-on-event-p event)
       (plusp (stack-midi:note-on-event-velocity event))))

(defun %filtered-smf (smf keep-p)
  "SMF with every track, every non-channel event, and the channel events for
which (funcall KEEP-P track-index event) is true, each track rebuilt from
absolute times, so times, the tempo map and the duration don't move."
  (stack-midi:make-smf
   :format (stack-midi:smf-format smf)
   :division (stack-midi:smf-division smf)
   :tracks (loop for track in (stack-midi:smf-tracks smf)
                 for i from 0
                 collect (let ((index i))
                           (stack-midi:track-from-absolute
                            (remove-if (lambda (timed)
                                         (let ((event (cdr timed)))
                                           (and (stack-midi:channel-event-p event)
                                                (not (funcall keep-p index event)))))
                                       (stack-midi:track-to-absolute track)))))))

(defun %track-name (track index)
  "The text of TRACK's first TRACK-NAME-EVENT, else \"Track INDEX\"."
  (let ((event (find-if #'stack-midi:track-name-event-p track)))
    (if event
        (stack-midi:text-meta-event-text event)
        (format nil "Track ~D" index))))

(defun %reference-notes (smf sample-rate)
  "The notes of SMF as fresh (n0 n1 key velocity) lists in note-on order: frames
n0 <= n < n1 sound KEY at VELOCITY. Events go in MERGE-TRACKS order; a note
ends at the first later note-off (or note-on of velocity 0) of its channel and
key that doesn't end an earlier note, else at the end of the SMF."
  (let ((division (stack-midi:smf-division smf))
        (tempo-map (stack-midi:tempo-map smf))
        (stop (seconds-to-frame (smf-duration smf) sample-rate))
        (open (make-array (* 16 128) :initial-element '())) ; queue per channel and key
        (notes '()))
    (flet ((frame (tick)
             (seconds-to-frame (stack-midi:ticks-to-seconds tick division tempo-map)
                               sample-rate))
           (voice (event key)
             (+ (* 128 (stack-midi:channel-event-channel event)) key)))
      (loop for (tick . event) in (stack-midi:track-to-absolute
                                   (stack-midi:merge-tracks (stack-midi:smf-tracks smf)))
            do (cond ((%note-p event)
                      (let* ((key (stack-midi:note-on-event-key event))
                             (note (list (frame tick) nil key
                                         (stack-midi:note-on-event-velocity event)))
                             (slot (voice event key)))
                        (push note notes)
                        (setf (svref open slot) (nconc (svref open slot) (list note)))))
                     ((or (stack-midi:note-off-event-p event)
                          (stack-midi:note-on-event-p event)) ; velocity 0
                      (let* ((key (if (stack-midi:note-on-event-p event)
                                      (stack-midi:note-on-event-key event)
                                      (stack-midi:note-off-event-key event)))
                             (note (pop (svref open (voice event key)))))
                        ;; A note-off with no open note is ignored.
                        (when note
                          (setf (second note) (frame tick))))))))
    (dolist (note notes)
      (unless (second note)
        (setf (second note) stop)))
    (nreverse notes)))

(defmethod render-midi :around ((engine audio-engine) smf
                                &key (sample-rate 44100) soundfont (channels 2) (tail 1))
  (let ((smf (%designated-smf smf)))
    ;; Inline, so a STORE-VALUE sets the variable passed on below.
    (check-type sample-rate (integer 1 #xFFFFFFFF))
    (check-type soundfont (or null pathname string))
    (check-type channels (member 1 2))
    (check-type tail (real 0))
    (%check-render-limit smf tail)
    (let ((buffer (call-next-method engine smf :sample-rate sample-rate :soundfont soundfont
                                               :channels channels :tail tail)))
      (unless (%render-shape-p buffer smf sample-rate channels tail)
        (error 'audio-engine-error
               :engine engine
               :reason (format nil "RENDER-MIDI returned ~S, not a buffer of ~D Hz, ~D ~
                                    channel~:P and ~D frames"
                               buffer sample-rate channels
                               (render-frames smf sample-rate tail))))
      buffer)))

(defmethod render-stems :around ((engine audio-engine) smf
                                 &key (sample-rate 44100) soundfont (channels 2) (tail 1)
                                 (by :track))
  (let ((smf (%designated-smf smf)))
    ;; Inline, so a STORE-VALUE sets the variable passed on below.
    (check-type sample-rate (integer 1 #xFFFFFFFF))
    (check-type soundfont (or null pathname string))
    (check-type channels (member 1 2))
    (check-type tail (real 0))
    (check-type by (member :track :channel))
    (%check-render-limit smf tail)
    (let ((stems (call-next-method engine smf :sample-rate sample-rate :soundfont soundfont
                                              :channels channels :tail tail :by by)))
      (unless (and (listp stems)
                   (null (cdr (last stems)))
                   (every (lambda (stem)
                            (and (stem-p stem)
                                 (%render-shape-p (stem-buffer stem) smf sample-rate
                                                  channels tail)))
                          stems))
        (error 'audio-engine-error
               :engine engine
               :reason (format nil "RENDER-STEMS returned something other than a list of ~
                                    stems of ~D Hz, ~D channel~:P and ~D frames"
                               sample-rate channels (render-frames smf sample-rate tail))))
      stems)))

(defmethod render-midi ((engine audio-engine) smf &key sample-rate soundfont channels tail)
  (declare (ignore smf sample-rate soundfont channels tail))
  (error 'audio-backend-unavailable :engine engine :reason "no render-midi method"))

(defmethod render-stems ((engine audio-engine) smf &key sample-rate soundfont channels tail by)
  (let ((tracks (stack-midi:smf-tracks smf)))
    (flet ((stem-buffer-for (keep-p)
             ;; Through the generic function, so its :AROUND checks each stem;
             ;; the limit was asked about once, for the whole render.
             (let ((*max-render-seconds* nil))
               (render-midi engine (%filtered-smf smf keep-p)
                            :sample-rate sample-rate :soundfont soundfont
                            :channels channels :tail tail))))
      (ecase by
        (:track
         (loop for track in tracks
               for i from 0
               when (some #'%note-p track)
                 collect (let ((index i))
                           (make-stem :name (%track-name track index)
                                      :buffer (stem-buffer-for
                                               (lambda (track-index event)
                                                 (declare (ignore event))
                                                 (= track-index index)))
                                      :track index))))
        (:channel
         (loop for channel from 0 below 16
               when (some (lambda (track)
                            (some (lambda (event)
                                    (and (%note-p event)
                                         (= channel (stack-midi:channel-event-channel event))))
                                  track))
                          tracks)
                 collect (let ((c channel))
                           (make-stem :name (format nil "Channel ~D" (1+ c))
                                      :buffer (stem-buffer-for
                                               (lambda (track-index event)
                                                 (declare (ignore track-index))
                                                 (= c (stack-midi:channel-event-channel event))))
                                      :channel c))))))))

(defmethod render-midi ((engine reference-engine) smf &key sample-rate soundfont channels tail)
  (declare (ignore soundfont))
  (let* ((frames (render-frames smf sample-rate tail))
         (acc (make-array frames :element-type 'double-float :initial-element 0d0))
         (amplitude (float (reference-engine-amplitude engine) 1d0))
         (square-p (eq :square (reference-engine-waveform engine)))
         (two-pi (* 2 (coerce pi 'double-float))))
    (loop for (n0 n1 key velocity) in (%reference-notes smf sample-rate)
          do (let ((f (note-frequency key))
                   (scale (* amplitude (/ velocity 127d0))))
               (loop for n from n0 below (min n1 frames)
                     ;; phi from the frame index, never a phase accumulator:
                     ;; the square wave flips exactly where the formula says.
                     do (let ((phi (/ (* f (- n n0)) sample-rate)))
                          (incf (aref acc n)
                                (* scale (if square-p
                                             (if (< (- phi (floor phi)) 1/2) 1d0 -1d0)
                                             (sin (* two-pi phi)))))))))
    (let ((y (%sample-vector (* frames channels))))
      (dotimes (n frames)
        (let ((s (coerce (aref acc n) 'single-float)))
          (dotimes (c channels)
            (setf (aref y (+ (* n channels) c)) s))))
      (%make-pcm-buffer sample-rate channels frames y))))
