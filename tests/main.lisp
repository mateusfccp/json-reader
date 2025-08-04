(in-package #:json-reader/tests)

;; Make sure that the reader is disabled even if the tests are interrupted for
;; some reason.
(teardown
  (json-reader-disable))

(deftest json-reader-enabled-value
  (json-reader-disable)

  (testing "*json-reader-enabled* should be nil by default."
    (ok (null *json-reader-enabled*)))

  (json-reader-enable)

  (testing "*json-reader-enabled* should be t after enabling the reader macro."
    (ng (null *json-reader-enabled*)))

  (json-reader-disable)

  (testing "*json-reader-enabled* should be nil after disabling the reader macro."
    (ok (null *json-reader-enabled*))))
