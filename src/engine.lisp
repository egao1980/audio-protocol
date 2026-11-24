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
  (declare (ignorable smf))
  (%au0-pending 'smf-duration))

(defun render-frames (smf sample-rate &optional (tail 1))
  "Frames of every render of SMF at SAMPLE-RATE with TAIL seconds after its end."
  (declare (ignorable smf sample-rate tail))
  (%au0-pending 'render-frames))

(defmethod render-midi :around ((engine audio-engine) smf
                                &key (sample-rate 44100) soundfont (channels 2) (tail 1))
  (declare (ignorable smf sample-rate soundfont channels tail))
  (%au0-pending 'render-midi))

(defmethod render-stems :around ((engine audio-engine) smf
                                 &key (sample-rate 44100) soundfont (channels 2) (tail 1)
                                   (by :track))
  (declare (ignorable smf sample-rate soundfont channels tail by))
  (%au0-pending 'render-stems))

(defmethod render-midi ((engine audio-engine) smf &key sample-rate soundfont channels tail)
  (declare (ignorable smf sample-rate soundfont channels tail))
  (%au0-pending 'render-midi))

(defmethod render-stems ((engine audio-engine) smf &key sample-rate soundfont channels tail by)
  (declare (ignorable smf sample-rate soundfont channels tail by))
  (%au0-pending 'render-stems))

(defmethod render-midi ((engine reference-engine) smf &key sample-rate soundfont channels tail)
  (declare (ignorable smf sample-rate soundfont channels tail))
  (%au0-pending 'render-midi))
