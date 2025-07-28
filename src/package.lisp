(uiop:define-package json-reader
    (:use #:cl
	  #:alexandria)
  (:export #:json-reader-enable
	   #:json-reader-disable
	   #:*reader-macro-enabled*))
