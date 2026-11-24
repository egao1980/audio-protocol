"""Generate the AU0 WAV test fixtures with Python's wave module and soundfile.

Run from the repo root:
    uv run --no-project --with soundfile==0.12.1 --with numpy==2.1.3 \
        python -I tests/fixtures/generate-au0.py

Writes the .wav files in FIXTURES, with the standard library's wave module
(16-bit PCM) and with soundfile 0.12.1 (BSD-3-Clause) over libsndfile 1.2.0
(LGPL-2.1, run out of process only). Every file is read back with soundfile,
and the 16-bit ones also with wave, and must give back the codes written;
au0-files.sexp records them. See PROVENANCE.md.

With --dump DIR it instead reads every .wav file in DIR (sorted by name) with
soundfile, checks the 16-bit ones against wave, and prints one s-expression:
((name encoding rate channels frames (code ...)) ...), the live cross-read.

Codes: a 16-bit sample is its signed integer, a 24-bit one its signed 24-bit
integer, a float32 one the unsigned 32-bit integer of its IEEE bits.
"""
import struct
import sys
import wave
from pathlib import Path

import numpy as np
import soundfile as sf

SOUNDFILE_VERSION = "0.12.1"
OUT = Path(__file__).resolve().parent
ENCODINGS = {"PCM_16": ":pcm16", "PCM_24": ":pcm24", "FLOAT": ":float32"}


class Lcg:
    """The 31-bit LCG of tests/package.lisp (RND), so values are reproducible."""

    def __init__(self, seed):
        self.seed = seed

    def rnd(self, n):
        self.seed = (self.seed * 1103515245 + 12345) % (2 ** 31)
        return (self.seed >> 8) % n


def codes16(rng, n):
    edge = [-32768, 32767, 0, -1, 1, -32767, 16384, -16384]
    return (edge + [rng.rnd(65536) - 32768 for _ in range(n)])[:n]


def codes24(rng, n):
    edge = [-8388608, 8388607, 0, -1, 1, 4194304, -8388607, 256]
    return (edge + [rng.rnd(16777216) - 8388608 for _ in range(n)])[:n]


def floats32(rng, n):
    edge = [0.0, -0.0, 1.0, -1.0, 0.5, 1.5, -2.0, 1.0e-3, 3.0e-41]
    vals = edge + [(rng.rnd(2000001) - 1000000) / 999983.0 for _ in range(n)]
    return np.array(vals[:n], dtype="<f4")


def float_bits(arr):
    return [int(b) for b in np.asarray(arr, dtype="<f4").view("<u4").ravel()]


# --- writers -----------------------------------------------------------------

def write_wave16(path, rate, channels, codes):
    with wave.open(str(path), "wb") as w:
        w.setnchannels(channels)
        w.setsampwidth(2)
        w.setframerate(rate)
        w.writeframes(struct.pack("<%dh" % len(codes), *codes))


def write_sf(path, rate, channels, encoding, values, fmt="WAV", meta=None):
    subtype = {":pcm16": "PCM_16", ":pcm24": "PCM_24", ":float32": "FLOAT"}[encoding]
    if encoding == ":pcm16":
        data = np.array(values, dtype="<i2")
    elif encoding == ":pcm24":
        # libsndfile maps int32 to 24-bit by dropping the low byte.
        data = np.array([v << 8 for v in values], dtype="<i4")
    else:
        data = np.asarray(values, dtype="<f4")
    data = data.reshape(-1, channels)
    with sf.SoundFile(str(path), "w", rate, channels, subtype, format=fmt) as f:
        for key, value in (meta or {}).items():
            setattr(f, key, value)
        f.write(data)


# --- readers -----------------------------------------------------------------

def read_sf(path):
    """(encoding rate channels frames codes) as soundfile reads PATH."""
    info = sf.info(str(path))
    encoding = ENCODINGS.get(info.subtype)
    if encoding is None:
        raise SystemExit("%s: unexpected subtype %s" % (path, info.subtype))
    if encoding == ":pcm16":
        data, _ = sf.read(str(path), dtype="int16", always_2d=True)
        codes = [int(v) for v in data.ravel()]
    elif encoding == ":pcm24":
        data, _ = sf.read(str(path), dtype="int32", always_2d=True)
        codes = [int(v) >> 8 for v in data.ravel()]
    else:
        data, _ = sf.read(str(path), dtype="float32", always_2d=True)
        codes = float_bits(data)
    return encoding, info.samplerate, info.channels, info.frames, codes


def check_wave16(path, codes):
    with wave.open(str(path), "rb") as w:
        raw = w.readframes(w.getnframes())
    got = list(struct.unpack("<%dh" % (len(raw) // 2), raw))
    if got != codes:
        raise SystemExit("%s: wave and soundfile disagree" % path)


def entry_sexp(name, encoding, rate, channels, frames, codes):
    return '("%s" %s %d %d %d (%s))' % (name, encoding, rate, channels, frames,
                                         " ".join(str(c) for c in codes))


# --- fixtures ----------------------------------------------------------------

def fixtures():
    """(name writer encoding rate channels values) for every fixture."""
    rng = Lcg(20261124)
    return [
        ("wave-pcm16-mono.wav", "wave", ":pcm16", 8000, 1, codes16(rng, 64)),
        ("wave-pcm16-stereo.wav", "wave", ":pcm16", 22050, 2, codes16(rng, 100)),
        ("sf-pcm16-stereo.wav", "sf", ":pcm16", 44100, 2, codes16(rng, 80)),
        ("sf-pcm24-mono-odd.wav", "sf", ":pcm24", 48000, 1, codes24(rng, 33)),
        ("sf-pcm24-stereo.wav", "sf", ":pcm24", 96000, 2, codes24(rng, 40)),
        ("sf-float32-stereo.wav", "sf", ":float32", 16000, 2, floats32(rng, 60)),
        ("sf-pcm16-info.wav", "sf-info", ":pcm16", 11025, 1, codes16(rng, 21)),
        ("sf-pcm16-wavex.wav", "sf-wavex", ":pcm16", 32000, 2, codes16(rng, 30)),
        ("sf-pcm24-wavex-3ch.wav", "sf-wavex", ":pcm24", 44100, 3, codes24(rng, 36)),
        ("sf-float32-wavex.wav", "sf-wavex", ":float32", 8000, 1, floats32(rng, 25)),
    ]


def generate():
    entries = []
    for name, writer, encoding, rate, channels, values in fixtures():
        path = OUT / name
        if writer == "wave":
            write_wave16(path, rate, channels, values)
        elif writer == "sf":
            write_sf(path, rate, channels, encoding, values)
        elif writer == "sf-info":
            write_sf(path, rate, channels, encoding, values,
                     meta={"title": "AU0 fixture", "artist": "cl-stack tests"})
        else:
            write_sf(path, rate, channels, encoding, values, fmt="WAVEX")
        expected = float_bits(values) if encoding == ":float32" else list(values)
        got = read_sf(path)
        want = (encoding, rate, channels, len(expected) // channels, expected)
        if got != want:
            raise SystemExit("%s: soundfile read back something else" % name)
        if encoding == ":pcm16":
            check_wave16(path, expected)
        entries.append(entry_sexp(name, *want))
    text = (";;;; Generated by generate-au0.py (soundfile %s): what each fixture holds,\n"
            ";;;; as soundfile reads it back. (name encoding rate channels frames codes)\n"
            "(\n%s)\n" % (SOUNDFILE_VERSION, "\n".join(" " + e for e in entries)))
    (OUT / "au0-files.sexp").write_text(text, encoding="utf-8")


def dump(directory):
    entries = []
    for path in sorted(Path(directory).glob("*.wav")):
        encoding, rate, channels, frames, codes = read_sf(path)
        if encoding == ":pcm16":
            check_wave16(path, codes)
        entries.append(entry_sexp(path.name, encoding, rate, channels, frames, codes))
    sys.stdout.write("(\n%s)\n" % "\n".join(entries))


if __name__ == "__main__":
    if sf.__version__ != SOUNDFILE_VERSION:
        raise SystemExit("soundfile %s, expected %s" % (sf.__version__, SOUNDFILE_VERSION))
    if len(sys.argv) == 3 and sys.argv[1] == "--dump":
        dump(sys.argv[2])
    elif len(sys.argv) == 1:
        generate()
    else:
        raise SystemExit("usage: generate-au0.py [--dump DIR]")
