(defpackage #:audio-protocol/tests
  (:use #:cl #:rove))

(in-package #:audio-protocol/tests)

(deftest skeleton-loads
  (ok (find-package '#:audio-protocol)))
