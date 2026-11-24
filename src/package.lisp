;;;; audio-protocol: batch audio for cl-stack. PCM buffers, WAV, mixing,
;;;; resampling, and the protocol of engines that render Standard MIDI Files.
;;;; Brief: cl-stack/docs/capabilities/audio.md, slice AU0. No real-time I/O,
;;;; no devices, no streaming: every operation takes whole buffers and returns
;;;; fresh ones.
;;;;
;;;; Files:
;;;;   conditions.lisp  the condition tree and the restart functions
;;;;   model.lisp       PCM-BUFFER, STEM, frames and seconds, gains in dB
;;;;   wav.lisp         READ-WAV, WRITE-WAV: RIFF/WAVE, PCM 16/24-bit, float32
;;;;   mix.lisp         PAN-GAINS (the pan law), MIX, LOOP-REGION
;;;;   resample.lisp    RESAMPLE: windowed sinc, exact rational positions
;;;;   engine.lisp      AUDIO-ENGINE, RENDER-MIDI, RENDER-STEMS, SMF-DURATION,
;;;;                    RENDER-FRAMES, and REFERENCE-ENGINE (a sine and square
;;;;                    synth that makes the protocol testable without a
;;;;                    SoundFont synthesiser)

(defpackage #:audio-protocol
  (:use #:cl)
  (:nicknames #:stack-audio)
  (:documentation "Batch audio for cl-stack (slice AU0).

A PCM-BUFFER holds interleaved SINGLE-FLOAT samples, nominally in -1..1, with a
sample rate and a channel count. READ-WAV and WRITE-WAV convert between buffers
and RIFF/WAVE bytes (PCM 16-bit and 24-bit, IEEE float32); 16-bit, 24-bit and
float32 samples all fit a SINGLE-FLOAT exactly, so reading and writing back in
the same encoding is lossless. MIX sums buffers or STEMs with a gain in dB, a
pan (PAN-GAINS states the law), mute, solo and a clipping policy. LOOP-REGION
repeats a passage. RESAMPLE changes the sample rate.

An AUDIO-ENGINE renders a midi-protocol SMF: RENDER-MIDI gives one buffer,
RENDER-STEMS one STEM per track or per channel, all of the same length.
REFERENCE-ENGINE is a tiny sine and square synth. SoundFont synthesis lives in
backends (audio-backend-fluidsynth).

Functions here never modify a buffer they are given.")
  (:export
   ;; Conditions and restarts (conditions.lisp)
   #:audio-error #:audio-error-reason
   #:audio-format-error #:audio-format-error-offset
   #:wav-unsupported-encoding #:wav-unsupported-encoding-format-tag
   #:wav-unsupported-encoding-bits
   #:audio-encode-error #:audio-encode-error-index
   #:audio-mismatch-error #:audio-mismatch-error-index
   #:audio-limit-exceeded #:audio-limit-exceeded-limit #:audio-limit-exceeded-value
   #:audio-engine-error #:audio-engine-error-engine
   #:audio-backend-unavailable
   #:audio-warning #:audio-warning-reason
   #:samples-clipped #:samples-clipped-count #:samples-clipped-peak
   #:wav-format-warning #:wav-format-warning-offset
   #:use-partial-data #:skip-input
   ;; Model (model.lisp)
   #:sample-vector
   #:pcm-buffer #:make-pcm-buffer #:pcm-buffer-p
   #:pcm-buffer-sample-rate #:pcm-buffer-channels #:pcm-buffer-frames
   #:pcm-buffer-samples #:pcm-buffer-duration
   #:sample-ref #:pcm-channel #:pcm-from-channels
   #:seconds-to-frame #:db-to-gain #:note-frequency
   #:stem #:make-stem #:stem-p #:stem-name #:stem-buffer #:stem-track #:stem-channel
   ;; WAV (wav.lisp)
   #:+wav-encodings+ #:read-wav #:write-wav
   ;; Mixing (mix.lisp)
   #:+clip-policies+ #:pan-gains #:mix #:loop-region
   ;; Resampling (resample.lisp)
   #:+resample-zero-crossings+ #:+resample-cutoff+ #:resample
   ;; Engines (engine.lisp)
   #:*max-render-seconds*
   #:audio-engine #:render-midi #:render-stems
   #:smf-duration #:render-frames
   #:reference-engine #:reference-engine-waveform #:reference-engine-amplitude))

(in-package #:audio-protocol)

(defun %au0-pending (name)
  "Contract stub for slice AU0. Every call is replaced by the implementation,
then this function is deleted."
  (error "audio-protocol AU0 pending: ~A" name))
