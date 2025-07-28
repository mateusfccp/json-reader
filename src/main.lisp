(in-package #:json-reader)

(defparameter *json-reader-enabled* nil
  "Whether the json-reader macro is enabled.")

(defparameter *old-readtable* nil
  "A place to store the original readtable before modification.")

(defmacro json-reader-enable ()
  "Enable the json-reader macro on READTABLE.

If it is already enabled, this functions is a no-op."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (when (null *json-reader-enabled*)
      (setf *old-readtable* *readtable*)
      (setf *readtable* (copy-readtable))

      (set-macro-character +left-bracket+ 'read-left-bracket)
      (set-macro-character +left-brace+ 'read-left-brace)
      ;; (set-macro-character +colon+ nil)
      (set-macro-character
       #\:
       (lambda (stream char)
	 (let ((first-char (peek-char nil stream nil nil t)))
	   (if (eq first-char #\Space)
	       nil
	       (let ((*readtable* *old-readtable*))
		 (unread-char +colon+ stream)
		 (read stream t nil t)
		 ))))
       t)

      (setf *json-reader-enabled* t))))

(defmacro json-reader-disable ()
  "Disable the json-reader macro.

If it is not enabled, this function is a no-op."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (unless (null *json-reader-enabled*)

      (setf *readtable* *old-readtable*)
      (makunbound '*old-readtable*)
      (setf *json-reader-enabled* nil))))
