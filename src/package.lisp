;;;; audio-protocol: batch audio for cl-stack. PCM buffers, WAV, mixing,
;;;; resampling, the protocol of engines that render Standard MIDI Files, and
;;;; a small DSP module. Brief: cl-stack/docs/capabilities/audio.md, slices AU0
;;;; and AU3. No real-time I/O, no devices, no streaming: every operation takes
;;;; whole buffers and returns fresh ones.
;;;;
;;;; Two packages, one system:
;;;;   AUDIO-PROTOCOL (STACK-AUDIO)          the model, WAV, mixing, engines (AU0)
;;;;   AUDIO-PROTOCOL/DSP (STACK-AUDIO/DSP)  FFT, windows, frames, levels, lags (AU3)
;;;; The DSP names (FFT, RMS, PEAK, NORMALIZE, ...) are common words, so they
;;;; live in their own package; a consumer may :USE both, since no symbol is
;;;; exported from both, or give the second a local nickname.
;;;;
;;;; Files:
;;;;   conditions.lisp       the condition tree and the restart functions
;;;;   model.lisp            PCM-BUFFER, STEM, frames and seconds, gains in dB
;;;;   wav.lisp              READ-WAV, WRITE-WAV: RIFF/WAVE, PCM 16/24-bit, float32
;;;;   mix.lisp              PAN-GAINS (the pan law), MIX, LOOP-REGION
;;;;   resample.lisp         RESAMPLE: windowed sinc, exact rational positions
;;;;   engine.lisp           AUDIO-ENGINE, RENDER-MIDI, RENDER-STEMS, SMF-DURATION,
;;;;                         RENDER-FRAMES, and REFERENCE-ENGINE (a sine and square
;;;;                         synth that makes the protocol testable without a
;;;;                         SoundFont synthesiser)
;;;;   fft.lisp              (DSP) FFT, IFFT, RFFT: radix 2; FFT-SIZE-ERROR, ZERO-PAD
;;;;   frames.lisp           (DSP) MAKE-WINDOW, FRAME-COUNT, FRAME-START,
;;;;                         FRAME-SIGNAL, STFT, MAGNITUDE, BIN-FREQUENCY
;;;;   levels.lisp           (DSP) RMS, PEAK, FRAME-RMS, NORMALIZE, DOWNMIX
;;;;   autocorrelation.lisp  (DSP) AUTOCORRELATION, YIN-DIFFERENCE,
;;;;                         CUMULATIVE-MEAN-NORMALIZE, YIN-CMNDF

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

The DSP functions of slice AU3 (FFT, windows, frames, STFT, levels, the YIN
lag functions) are in the package AUDIO-PROTOCOL/DSP (STACK-AUDIO/DSP).

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

(defpackage #:audio-protocol/dsp
  (:use #:cl #:audio-protocol)
  (:nicknames #:stack-audio/dsp)
  (:documentation "DSP basics for audio-protocol (slice AU3): what a pitch
tracker or another analysis front end needs, in pure Lisp.

FFT, IFFT and RFFT are radix-2 discrete Fourier transforms of power-of-two
sizes (FFT-SIZE-ERROR otherwise, with the restarts ZERO-PAD and USE-VALUE).
MAKE-WINDOW gives Hann, Hamming and Blackman windows, periodic by default.
FRAME-SIGNAL cuts a mono signal into analysis frames of a size and a hop, with
a padding policy at the ends; STFT transforms each windowed frame. RMS, PEAK,
FRAME-RMS and NORMALIZE measure and scale levels of PCM-BUFFERs and plain
vectors; DOWNMIX makes a buffer mono. AUTOCORRELATION and YIN-DIFFERENCE (with
CUMULATIVE-MEAN-NORMALIZE and YIN-CMNDF) are the lag functions of the YIN
family, computed through the FFT in O(N log N).

Everything computes in DOUBLE-FLOAT, returns fresh DOUBLE-VECTORs and
COMPLEX-VECTORs (and PCM-BUFFERs where a buffer goes in), and never modifies an
argument.")
  (:export
   ;; Types and the size condition (fft.lisp)
   #:double-vector #:complex-vector
   #:fft-size-error #:fft-size-error-size #:zero-pad
   ;; FFT (fft.lisp)
   #:power-of-two-p #:next-power-of-two #:fft #:ifft #:rfft
   ;; Windows, frames, STFT (frames.lisp)
   #:+window-kinds+ #:+pad-policies+ #:make-window
   #:frame-count #:frame-start #:frame-signal
   #:stft #:magnitude #:bin-frequency
   ;; Levels (levels.lisp)
   #:rms #:peak #:frame-rms #:normalize #:downmix
   ;; Lags (autocorrelation.lisp)
   #:autocorrelation #:yin-difference #:cumulative-mean-normalize #:yin-cmndf))

(in-package #:audio-protocol)
