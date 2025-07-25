(uiop:define-package json-reader
    (:use #:cl)
  (:export #:json-reader-enable
	   #:json-reader-disable
	   #:*reader-macro-enabled*))
