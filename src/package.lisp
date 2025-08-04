(uiop:define-package json-reader
    (:use #:cl
	  #:alexandria
	  #:serapeum)
  (:export #:json-reader-enable
	   #:json-reader-disable
	   #:*json-reader-enabled*))
