(defsystem "audio-protocol"
  :version "0.0.1"
  :description "PCM buffers, WAV, mixing and MIDI rendering engines for cl-stack (batch audio)"
  :author "egao1980"
  :license "MIT"
  :pathname "src"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "audio-protocol/tests"))))

(defsystem "audio-protocol/tests"
  :depends-on ("audio-protocol" (:version "rove" "0.11.1"))
  :pathname "tests"
  :serial t
  :components ((:file "audio-protocol-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
