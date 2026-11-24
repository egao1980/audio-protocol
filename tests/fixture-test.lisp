;;;; AU0 WAV against files Python's wave module and soundfile 0.12.1 wrote
;;;; (tests/fixtures/PROVENANCE.md): read as soundfile reads them, and the plain
;;;; PCM ones written back byte for byte.

(in-package #:audio-protocol/tests)

(defparameter +byte-exact-fixtures+
  '("wave-pcm16-mono.wav" "wave-pcm16-stereo.wav" "sf-pcm16-stereo.wav"
    "sf-pcm24-mono-odd.wav" "sf-pcm24-stereo.wav")
  "Fixtures in WRITE-WAV's own layout: 16-bit files of wave and libsndfile, and
libsndfile's plain 24-bit files (one with an odd data size and its pad byte).")

(deftest fixture-table
  (ok (= 10 (length *fixtures*)))
  (ok (every (lambda (entry) (probe-file (fixture-path (first entry)))) *fixtures*))
  (ok (every (lambda (name) (assoc name *fixtures* :test #'string=)) +byte-exact-fixtures+)))

(deftest-parametrize reads-what-soundfile-read
    ((name) :rows (mapcar #'first *fixtures*))
  (destructuring-bind (encoding rate channels frames codes) (rest (fixture-entry name))
    (multiple-value-bind (b got warnings) (read-with-warnings (fixture-octets name))
      (ok (eq encoding got))
      (ok (= rate (pcm-buffer-sample-rate b)))
      (ok (= channels (pcm-buffer-channels b)))
      (ok (= frames (pcm-buffer-frames b)))
      (ok (equal codes (buffer-codes b encoding)) "every sample")
      (ok (null warnings)))))

(deftest-parametrize writes-back-byte-for-byte
    ((name) :rows +byte-exact-fixtures+)
  (let ((bytes (fixture-octets name)))
    (multiple-value-bind (b encoding) (read-wav bytes)
      (ok (equalp bytes (write-wav b nil :encoding encoding))))))

(deftest-parametrize every-fixture-survives-a-rewrite
    ((name) :rows (mapcar #'first *fixtures*))
  (multiple-value-bind (b encoding) (read-wav (fixture-octets name))
    (let ((again (read-wav (write-wav b nil :encoding encoding))))
      (ok (equalp b again))
      (ok (equal (buffer-codes b encoding) (buffer-codes again encoding))))))

(deftest the-float-fixture-goes-beyond-one
  (let ((b (read-wav (fixture-octets "sf-float32-stereo.wav"))))
    (ok (find 1.5 (pcm-buffer-samples b)))
    (ok (find -2.0 (pcm-buffer-samples b)))))
