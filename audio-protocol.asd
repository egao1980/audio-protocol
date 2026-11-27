(defsystem "audio-protocol"
  :version "0.2.0"
  :description "Batch audio for cl-stack: PCM buffers, WAV read and write, mixing (gain, pan, mute, solo, clipping), loops, resampling, engines that render Standard MIDI Files to buffers or stems, and DSP basics (radix-2 FFT, windows, frames, STFT, levels, autocorrelation and the YIN difference functions)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "binary-protocol" "0.1.1")
               (:version "midi-protocol" "0.1.0"))
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "model")
               (:file "wav")
               (:file "mix")
               (:file "resample")
               (:file "engine")
               (:file "fft")
               (:file "frames")
               (:file "levels")
               (:file "autocorrelation"))
  :in-order-to ((test-op (test-op "audio-protocol/tests"))))

(defsystem "audio-protocol/tests"
  :depends-on ("audio-protocol" (:version "rove" "0.11.1") "cl-stack-pathlib"
               "process-backend-uiop")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "model-test")
               (:file "wav-test")
               (:file "malformed-test")
               (:file "fixture-test")
               (:file "soundfile-test")
               (:file "mix-test")
               (:file "loop-test")
               (:file "resample-test")
               (:file "engine-test")
               (:file "stems-test")
               (:file "dsp-support")
               (:file "fft-test")
               (:file "frames-test")
               (:file "levels-test")
               (:file "autocorrelation-test")
               (:file "dsp-pipeline-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
