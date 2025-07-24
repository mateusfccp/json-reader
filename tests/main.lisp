(defpackage json-reader/tests/main
  (:use :cl
        :json-reader
        :rove))
(in-package :json-reader/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :json-reader)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
