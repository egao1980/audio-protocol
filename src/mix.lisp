;;;; AU0 mixing: the pan law, MIX, LOOP-REGION.
;;;;
;;;; Arithmetic is DOUBLE-FLOAT: every sample is read as a double, products and
;;;; sums are doubles, and each output sample is rounded once to SINGLE-FLOAT.
;;;; pi below is CL:PI as a DOUBLE-FLOAT; a pan or gain is converted to a
;;;; DOUBLE-FLOAT first.
;;;;
;;;; (PAN-GAINS pan input-channels) -> (values left right), DOUBLE-FLOATs.
;;;;   PAN is a real in -1 (left) .. 1 (right); INPUT-CHANNELS is 1 or 2 (else
;;;;   TYPE-ERROR).
;;;;   1, a mono input placed in a stereo mix: constant power, sine law,
;;;;        left = sin((1 - pan) * pi/4), right = sin((1 + pan) * pi/4).
;;;;      left^2 + right^2 = 1 for every pan, so the centre is -3.01 dB per side
;;;;      (sin(pi/4) = 0.7071...) and a source keeps its loudness as it moves.
;;;;      Hard left (-1) is exactly (1 0), hard right exactly (0 1).
;;;;   2, a stereo input: balance, each side's own signal attenuated linearly,
;;;;        left = min(1, 1 - pan), right = min(1, 1 + pan).
;;;;      The centre is (1 1), so a stereo input mixed at pan 0 is unchanged.
;;;;
;;;; (MIX inputs &key gains pans mute solo (master 0) (clip :clip) (channels 2))
;;;;   -> a fresh PCM-BUFFER.
;;;; Arguments (TYPE-ERROR for a wrong type):
;;;;   INPUTS   a non-empty list of PCM-BUFFERs and STEMs (a STEM mixes its BUFFER).
;;;;   GAINS    NIL (every input at 0 dB) or a list of reals, dB, one per input.
;;;;   PANS     NIL (every input at 0) or a list of reals in -1..1, one per input.
;;;;   MUTE     NIL or a list of generalized booleans, one per input.
;;;;   SOLO     NIL or a list of generalized booleans, one per input.
;;;;   MASTER   a real, dB, applied to the sum.
;;;;   CLIP     a keyword of +CLIP-POLICIES+.
;;;;   CHANNELS 1 or 2: the output's channel count.
;;;; A list among GAINS, PANS, MUTE, SOLO whose length isn't (length INPUTS)
;;;; signals AUDIO-ERROR. Entries line up with INPUTS by position.
;;;;
;;;; Inputs, checked in order. The first input left in sets the sample rate.
;;;; An input whose sample rate differs, or whose channel count isn't 1 or 2,
;;;; signals AUDIO-MISMATCH-ERROR (INDEX its position in INPUTS) inside two
;;;; restarts: SKIP-INPUT leaves it out (its GAINS, PANS, MUTE and SOLO entries
;;;; with it), USE-VALUE replaces it with the value, a PCM-BUFFER or STEM, which
;;;; is checked again. When every input was left out: AUDIO-ERROR.
;;;;
;;;; Mute and solo. Input i is heard when it isn't muted and, if any input
;;;; left in is soloed, it is soloed too. So mute wins over solo, and soloing
;;;; only muted inputs gives silence.
;;;;
;;;; Length. The output has F frames, the largest frame count of the inputs
;;;; left in, heard or not (muting never changes the length). An input is
;;;; silent after its own last frame.
;;;;
;;;; Sum. With G_i = (db-to-gain gain_i), M = (db-to-gain master) and the
;;;; input's matrix A_i, output channel c of frame n is
;;;;     y = M * sum over heard inputs i (in order), over input channels k (in
;;;;             order), of (G_i * A_i[c][k]) * x_i[n][k]
;;;; rounded to a SINGLE-FLOAT, where
;;;;   CHANNELS 2: mono input   A = column (left right) of (PAN-GAINS pan 1);
;;;;               stereo input A = diagonal (left right) of (PAN-GAINS pan 2).
;;;;   CHANNELS 1: mono input   A = (1); stereo input A = (1/2 1/2). PANS is
;;;;               ignored.
;;;; So one stereo input at 0 dB and pan 0 comes out sample for sample.
;;;;
;;;; Clipping, on the rounded y, after the whole sum:
;;;;   :CLIP   each y is clamped to -1..1. When any |y| > 1, one SAMPLES-CLIPPED
;;;;           warning (COUNT those samples, PEAK the largest |y| before
;;;;           clamping), with MUFFLE-WARNING. 1.0 itself is not clipped.
;;;;   :SCALE  when the peak P (largest |y|) is above 1, every y becomes
;;;;           y / P, rounded to a SINGLE-FLOAT, so the peak is exactly 1. No
;;;;           warning.
;;;;   :NONE   y is left as it is (a float32 file keeps it).
;;;;
;;;; (LOOP-REGION buffer start end &key (count 2) (fade 0) (gap 0))
;;;;   -> a fresh PCM-BUFFER, BUFFER's rate and channels: a passage repeated,
;;;;   for practice.
;;;;   START and END are frame indices, integers with 0 <= START < END <=
;;;;   frames; the region is frames START .. END-1, N = END - START frames.
;;;;   COUNT, an integer >= 1, is how many times it plays; GAP, an integer >= 0,
;;;;   the frames of silence between two plays; FADE, an integer >= 0 with
;;;;   2 * FADE <= N, the length of a linear fade in and out of every play.
;;;;   Wrong types are TYPE-ERRORs; values outside these bounds AUDIO-ERRORs.
;;;;   The output has COUNT * N + (COUNT - 1) * GAP frames; play j (from 0)
;;;;   starts at frame j * (N + GAP). Frame k (0 <= k < N) of a play is region
;;;;   frame k, every channel multiplied by
;;;;       g(k) = min(1, k / FADE, (N - 1 - k) / FADE)    (FADE > 0; else 1)
;;;;   an exact rational, taken as a DOUBLE-FLOAT. With FADE 0 every play is an
;;;;   exact copy; with FADE > 0 the first and last frame of each play are 0.

(in-package #:audio-protocol)

(defparameter +clip-policies+ '(:clip :scale :none)
  "The values of MIX's :CLIP. Data.")

(defun pan-gains (pan input-channels)
  "(values left right) for PAN (-1..1) of an input of INPUT-CHANNELS (1: the
constant-power sine law; 2: balance). See the file header."
  (declare (ignorable pan input-channels))
  (%au0-pending 'pan-gains))

(defun mix (inputs &key gains pans mute solo (master 0) (clip :clip) (channels 2))
  "Sum INPUTS (PCM-BUFFERs and STEMs) into a fresh buffer of CHANNELS (1 or 2),
with a gain in dB, a pan, mute and solo per input, a MASTER gain in dB and the
CLIP policy. Signals AUDIO-MISMATCH-ERROR (SKIP-INPUT, USE-VALUE) and
SAMPLES-CLIPPED. See the file header."
  (declare (ignorable inputs gains pans mute solo master clip channels))
  (%au0-pending 'mix))

(defun loop-region (buffer start end &key (count 2) (fade 0) (gap 0))
  "Frames START..END-1 of BUFFER played COUNT times, GAP frames of silence
between plays, each faded in and out over FADE frames. See the file header."
  (declare (ignorable buffer start end count fade gap))
  (%au0-pending 'loop-region))
