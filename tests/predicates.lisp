(in-package #:json-reader/tests)

(deftest pairp
  (testing "A pair is a cons cell with a car and a non-list cdr."
    (ok (pairp '("foo" . "bar")))
    (ng (pairp '("foo" . ("bar" . "baz"))))
    (ng (pairp nil))
    (ng (pairp '("foo" "bar")))))
