(in-package #:json-reader)

(defparameter *json-reader-enabled* nil
  "Whether the json-reader macro is enabled.")

(defparameter *old-readtable* nil
  "A place to store the original readtable before modification.")
