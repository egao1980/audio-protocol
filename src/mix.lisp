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
  (check-type pan (real -1 1))
  (check-type input-channels (member 1 2))
  (let ((p (float pan 1d0)))
    (if (= input-channels 1)
        (let ((quarter-pi (/ (coerce pi 'double-float) 4)))
          (values (sin (* (- 1 p) quarter-pi))
                  (sin (* (+ 1 p) quarter-pi))))
        ;; 1D0, not 1: MIN would return the integer at the centre.
        (values (min 1d0 (- 1d0 p))
                (min 1d0 (+ 1d0 p))))))

(defun %check-elements (list type)
  "Signal a TYPE-ERROR for the first element of LIST that isn't of TYPE. (A
CHECK-TYPE on an element would offer a STORE-VALUE into the caller's list.)"
  (dolist (x list)
    (unless (typep x type)
      (error 'type-error :datum x :expected-type type))))

(defun %check-length (list what count)
  "AUDIO-ERROR unless LIST is NIL or has COUNT elements, one per input."
  (when (and list (/= (length list) count))
    (error 'audio-error
           :reason (format nil "~D ~A for ~D input~:P" (length list) what count))))

(defun %input-buffer (input)
  "The PCM-BUFFER of a MIX input: a buffer, or a STEM's buffer."
  (if (stem-p input) (stem-buffer input) input))

(defun %mismatch (buffer rate)
  "Why BUFFER can't join a mix at RATE (NIL: no input is in yet), or NIL."
  (let ((channels (pcm-buffer-channels buffer))
        (own-rate (pcm-buffer-sample-rate buffer)))
    (cond ((not (member channels '(1 2)))
           (format nil "~D channels; MIX takes 1 or 2" channels))
          ((and rate (/= rate own-rate))
           (format nil "sample rate ~D, not ~D" own-rate rate)))))

(defun %kept-inputs (inputs gains pans mute solo)
  "(values entries rate): an entry (buffer gain pan mute solo) for each input
left in, in order, and the sample rate the first of them set. An input that
doesn't fit signals AUDIO-MISMATCH-ERROR with SKIP-INPUT (its entry goes) and
USE-VALUE (the value replaces it and is checked again)."
  (let ((rate nil)
        (kept '()))
    (loop for input in inputs
          for i from 0
          for gain in gains
          for pan in pans
          for m in mute
          for s in solo
          do (let ((candidate input))
               (loop
                 (let* ((buffer (%input-buffer candidate))
                        (problem (%mismatch buffer rate)))
                   (unless problem
                     (setf rate (or rate (pcm-buffer-sample-rate buffer)))
                     (push (list buffer gain pan m s) kept)
                     (return))
                   (restart-case (error 'audio-mismatch-error :index i :reason problem)
                     (skip-input ()
                       :report "Leave this input out of the mix."
                       (return))
                     (use-value (value)
                       :report "Mix another PCM-BUFFER or STEM in its place."
                       :interactive (lambda () (%read-restart-value "input (a PCM-BUFFER or STEM)"))
                       (check-type value (or pcm-buffer stem))
                       (setf candidate value)))))))
    (unless kept
      (error 'audio-error :reason "every input was left out"))
    (values (nreverse kept) rate)))

(defun %mix-coefficients (gain pan in-channels out-channels)
  "The OUT-CHANNELS x IN-CHANNELS DOUBLE-FLOAT matrix of G * A[c][k] in the
file header's sum, G the gain of GAIN dB and A the input's matrix."
  (let ((g (db-to-gain gain))
        (m (make-array (list out-channels in-channels)
                       :element-type 'double-float :initial-element 0d0)))
    (if (= out-channels 1)
        (dotimes (k in-channels)
          (setf (aref m 0 k) (* g (if (= in-channels 1) 1d0 0.5d0))))
        (multiple-value-bind (left right) (pan-gains pan in-channels)
          (if (= in-channels 1)
              (setf (aref m 0 0) (* g left)
                    (aref m 1 0) (* g right))
              (setf (aref m 0 0) (* g left)
                    (aref m 1 1) (* g right)))))
    m))

(defun %mix-into (acc out-channels buffer coefficients)
  "Add BUFFER through COEFFICIENTS into ACC, a DOUBLE-FLOAT vector of interleaved
OUT-CHANNELS frames, input channel by input channel."
  (let ((x (pcm-buffer-samples buffer))
        (in-channels (pcm-buffer-channels buffer)))
    (dotimes (n (pcm-buffer-frames buffer))
      (dotimes (c out-channels)
        (let ((i (+ (* n out-channels) c)))
          (dotimes (k in-channels)
            (incf (aref acc i)
                  (* (aref coefficients c k) (aref x (+ (* n in-channels) k))))))))))

(defun %round-and-clip (acc master clip)
  "A fresh SAMPLE-VECTOR: M * ACC rounded once to SINGLE-FLOAT (M the gain of
MASTER dB), then the CLIP policy on the rounded samples."
  (let ((m (db-to-gain master))
        (y (%sample-vector (length acc))))
    (dotimes (i (length acc))
      (setf (aref y i) (coerce (* m (aref acc i)) 'single-float)))
    (ecase clip
      (:none)
      (:clip
       (let ((count 0)
             (peak 0f0))
         (dotimes (i (length y))
           (let ((a (abs (aref y i))))
             (when (> a 1f0)
               (incf count)
               (setf peak (max peak a)
                     (aref y i) (float-sign (aref y i) 1f0)))))
         (when (plusp count)
           (warn 'samples-clipped :count count :peak peak
                                  :reason "the mix was clamped to -1..1"))))
      (:scale
       (let ((peak (reduce #'max y :key #'abs :initial-value 0f0)))
         (when (> peak 1f0)
           (let ((p (float peak 1d0)))
             (dotimes (i (length y))
               (setf (aref y i) (coerce (/ (aref y i) p) 'single-float))))))))
    y))

(defun mix (inputs &key gains pans mute solo (master 0) (clip :clip) (channels 2))
  "Sum INPUTS (PCM-BUFFERs and STEMs) into a fresh buffer of CHANNELS (1 or 2),
with a gain in dB, a pan, mute and solo per input, a MASTER gain in dB and the
CLIP policy. Signals AUDIO-MISMATCH-ERROR (SKIP-INPUT, USE-VALUE) and
SAMPLES-CLIPPED. See the file header."
  (check-type inputs cons)
  (%check-elements inputs '(or pcm-buffer stem))
  (check-type gains list)
  (%check-elements gains 'real)
  (check-type pans list)
  (%check-elements pans '(real -1 1))
  (check-type mute list)
  (check-type solo list)
  (check-type master real)
  (check-type clip (member :clip :scale :none)) ; +CLIP-POLICIES+
  (check-type channels (member 1 2))
  (let ((count (length inputs)))
    (%check-length gains "gains" count)
    (%check-length pans "pans" count)
    (%check-length mute "mute flags" count)
    (%check-length solo "solo flags" count)
    (flet ((or-default (list value)
             (or list (make-list count :initial-element value))))
      (multiple-value-bind (kept rate)
          (%kept-inputs inputs (or-default gains 0) (or-default pans 0)
                        (or-default mute nil) (or-default solo nil))
        (let* ((frames (reduce #'max kept :key (lambda (entry) (pcm-buffer-frames (first entry)))))
               (any-solo (some #'fifth kept))
               (acc (make-array (* frames channels) :element-type 'double-float
                                                    :initial-element 0d0)))
          (loop for (buffer gain pan muted soloed) in kept
                ;; Heard: not muted and, when any input left in is soloed,
                ;; soloed too. Mute wins.
                when (and (not muted) (or (not any-solo) soloed))
                  do (%mix-into acc channels buffer
                                (%mix-coefficients gain pan (pcm-buffer-channels buffer)
                                                   channels)))
          (%make-pcm-buffer rate channels frames (%round-and-clip acc master clip)))))))

(defun loop-region (buffer start end &key (count 2) (fade 0) (gap 0))
  "Frames START..END-1 of BUFFER played COUNT times, GAP frames of silence
between plays, each faded in and out over FADE frames. See the file header."
  (check-type buffer pcm-buffer)
  (check-type start integer)
  (check-type end integer)
  (check-type count integer)
  (check-type fade integer)
  (check-type gap integer)
  (let ((frames (pcm-buffer-frames buffer))
        (channels (pcm-buffer-channels buffer)))
    (flet ((out-of-bounds (control &rest arguments)
             (error 'audio-error :reason (apply #'format nil control arguments))))
      (unless (<= 0 start)
        (out-of-bounds "START ~D is negative" start))
      (unless (< start end)
        (out-of-bounds "END ~D is not after START ~D" end start))
      (unless (<= end frames)
        (out-of-bounds "END ~D is past the buffer's ~D frames" end frames))
      (unless (<= 1 count)
        (out-of-bounds "COUNT ~D is below 1" count))
      (unless (<= 0 gap)
        (out-of-bounds "GAP ~D is negative" gap))
      (unless (<= 0 (* 2 fade) (- end start))
        (out-of-bounds "FADE ~D doesn't fit twice in ~D frames" fade (- end start))))
    (let* ((n (- end start))
           (x (pcm-buffer-samples buffer))
           (out-frames (+ (* count n) (* (1- count) gap)))
           (y (%sample-vector (* out-frames channels)))
           (gains (make-array n :element-type 'double-float)))
      (dotimes (k n)
        (setf (aref gains k)
              (float (if (zerop fade) 1 (min 1 (/ k fade) (/ (- n 1 k) fade))) 1d0)))
      (dotimes (j count)
        (let ((base (* j (+ n gap))))
          (dotimes (k n)
            (dotimes (c channels)
              (setf (aref y (+ (* (+ base k) channels) c))
                    (coerce (* (aref gains k)
                               (float (aref x (+ (* (+ start k) channels) c)) 1d0))
                            'single-float))))))
      (%make-pcm-buffer (pcm-buffer-sample-rate buffer) channels out-frames y))))
