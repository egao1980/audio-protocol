# audio-protocol

Batch audio for cl-stack: PCM buffers, WAV read and write, mixing (gain, pan, mute and solo), resampling, and a protocol for engines that render Standard MIDI Files to audio, one buffer or one stem per track. There is no real-time audio I/O and no device access. MIDI files come from [midi-protocol](https://github.com/egao1980/midi-protocol); byte I/O builds on [binary-protocol](https://github.com/egao1980/binary-protocol).

**Status:** contract for slice AU0 (0.1.0). The design is in the capability brief [cl-stack/docs/capabilities/audio.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/audio.md). Each file header in `src/` states its rules, the functions are stubs, and the Rove suite in `tests/` is the spec. A team-solve run implements them.

| File | What |
|------|------|
| `src/model.lisp` | `pcm-buffer` (interleaved `single-float`), `stem`, frames and seconds, gains in dB |
| `src/wav.lisp` | `read-wav`, `write-wav`: RIFF/WAVE, PCM 16 and 24-bit, float32, WAVE_FORMAT_EXTENSIBLE on read |
| `src/mix.lisp` | `pan-gains` (sine law for mono, balance for stereo), `mix` (gain, pan, mute, solo, clipping), `loop-region` |
| `src/resample.lisp` | `resample`: Blackman-windowed sinc at exact rational positions |
| `src/engine.lisp` | `audio-engine`, `render-midi`, `render-stems`, and `reference-engine`, a sine and square synth |

```sh
cl-repo test --system audio-protocol
```

## License

MIT, see [LICENSE](LICENSE).
