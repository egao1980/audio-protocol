# audio-protocol

Batch audio for cl-stack: PCM buffers, WAV read and write, mixing (gain, pan, mute and solo), resampling, a protocol for engines that render Standard MIDI Files to audio (one buffer, or one stem per track), and DSP basics for analysis front ends: FFT, windows, frames, STFT, levels, and the YIN lag functions. There is no real-time audio I/O and no device access. MIDI files come from [midi-protocol](https://github.com/egao1980/midi-protocol); byte I/O builds on [binary-protocol](https://github.com/egao1980/binary-protocol).

**Status:** slice AU0 (0.1.0) is implemented against its contract. Slice AU3 (0.2.0, the package `audio-protocol/dsp`, nickname `stack-audio/dsp`) is a **contract**: its functions are stubs, and a team-solve run implements them. The design is in the capability brief [cl-stack/docs/capabilities/audio.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/audio.md). Each file header in `src/` states its rules, and the Rove suite in `tests/` is the spec.

| File | What | Status |
|------|------|--------|
| `src/model.lisp` | `pcm-buffer` (interleaved `single-float`), `stem`, frames and seconds, gains in dB | AU0, implemented |
| `src/wav.lisp` | `read-wav`, `write-wav`: RIFF/WAVE, PCM 16 and 24-bit, float32, WAVE_FORMAT_EXTENSIBLE on read | AU0, implemented |
| `src/mix.lisp` | `pan-gains` (sine law for mono, balance for stereo), `mix` (gain, pan, mute, solo, clipping), `loop-region` | AU0, implemented |
| `src/resample.lisp` | `resample`: Blackman-windowed sinc at exact rational positions | AU0, implemented |
| `src/engine.lisp` | `audio-engine`, `render-midi`, `render-stems`, and `reference-engine`, a sine and square synth | AU0, implemented |
| `src/fft.lisp` | `fft`, `ifft`, `rfft`: radix 2, power-of-two sizes; `fft-size-error` with the restarts `zero-pad` and `use-value` | AU3, contract |
| `src/frames.lisp` | `make-window` (Hann, Hamming, Blackman; periodic by default), `frame-count`, `frame-start`, `frame-signal` (`:center`, `:end`, `:none`), `stft`, `magnitude`, `bin-frequency` | AU3, contract |
| `src/levels.lisp` | `rms`, `peak`, `frame-rms`, `normalize` (to a peak in dBFS), `downmix` | AU3, contract |
| `src/autocorrelation.lisp` | `autocorrelation`, `yin-difference`, `cumulative-mean-normalize`, `yin-cmndf`: through the FFT, O(N log N) | AU3, contract |

```sh
cl-repo test --system audio-protocol
```

## License

MIT, see [LICENSE](LICENSE).
