(in-package #:json-reader)

(declaim (boolean *json-reader-enabled*))
(defparameter *json-reader-enabled* nil
  "Whether the json-reader macro is enabled.")

(declaim ((or readtable null) *old-readtable*))
(defparameter *old-readtable* nil
  "A place to store the original readtable before modification.")
