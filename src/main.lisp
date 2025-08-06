(in-package #:json-reader)

(defmacro json-reader-enable ()
  "Enable the json-reader macro on READTABLE.

If it is already enabled, this functions is a no-op."
  '(eval-always
    (when (null *json-reader-enabled*)
      (setf *old-readtable* *readtable*)
      (setf *readtable* (copy-readtable))

      (set-macro-character +colon+ 'read-colon t)
      (set-macro-character +left-bracket+ 'read-left-bracket)
      (set-macro-character +left-brace+ 'read-left-brace)

      (setf *json-reader-enabled* t))))

(defmacro json-reader-disable ()
  "Disable the json-reader macro.

If it is not enabled, this function is a no-op."
  '(eval-always
    (unless (null *json-reader-enabled*)

      (setf *readtable* *old-readtable*)
      (setf *old-readtable* nil)
      (setf *json-reader-enabled* nil))))
