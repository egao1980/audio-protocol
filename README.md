# audio-protocol

Batch audio for cl-stack: PCM buffers, WAV read and write, mixing (gain, pan, mute and solo), resampling, and a protocol for engines that render Standard MIDI Files to audio, one buffer or one stem per track. There is no real-time audio I/O and no device access. MIDI files come from [midi-protocol](https://github.com/egao1980/midi-protocol); byte I/O builds on [binary-protocol](https://github.com/egao1980/binary-protocol).

**Status:** skeleton (0.0.1). The design is in the capability brief [cl-stack/docs/capabilities/audio.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/audio.md) (slice AU0). The API and tests land through contract branches and team-solve.

```sh
cl-repo test --system audio-protocol
```

## License

MIT, see [LICENSE](LICENSE).
