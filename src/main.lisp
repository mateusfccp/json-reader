(uiop:define-package json-reader
    (:use #:cl))

(in-package #:json-reader)

(defun create-json-hash-table (&rest pairs)
  "Creates a HASH-TABLE based on a list of PAIRS."
  ;; (declare (list pairs))
  (format t "Creating hash for pairs: ~A~&" pairs)
  (let ((hash-table (make-hash-table :test #'equal)))
    (loop for (key . value) in pairs
	  do (setf (gethash key hash-table) value))
    hash-table))

(defun normalize-json-value (value)
  "Normalize a JSON VALUE to a LISP equivalent."
  (cond
    ((symbolp value)
     (case value
       (true t)
       (false '(nil))
       (null '('null))
       (otherwise value)))
    ((and (consp value)
	  (not (consp (cdr value))))
     `(',value))
    (t
     (eval value))))

(defun read-left-bracket (stream char)
  "Reads the left bracket character and parses the remaining STREAM with
READ-NEXT-OBJECT.

This will try to parse the given STREAM like a JSON list, and will return a
VECTOR with the internal elements."
  (declare (ignore char))
  (let ((elements (read-separated-list +right-bracket+
				       :input-stream stream
				       :recursive-p t)))
    (loop
      with normalized-element = nil
      for element in elements
      do (setf normalized-element (normalize-json-value element))
      if (listp normalized-element)
	append normalized-element into result
      else
	collect normalized-element into result
      finally
	 (return `(vector ,@result)))))

(set-macro-character +left-bracket+ 'read-left-bracket)

(defun read-left-brace (stream char)
  "Reads the left brace character and parses the remaining STREAM with
READ-NEXT-OBJECT.

This will try to parse the given STREAM like a JSON object, and will return a
HASH-TABLE with the internal elements."
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (reserve-character +colon+)
    (let ((elements (read-separated-list +right-brace+
					 :input-stream stream
					 :recursive-p t)))
      (loop
	with normalized-element = nil
	for element in elements
	do (setf normalized-element (normalize-json-value element))
	if (listp normalized-element)
	  append normalized-element into result
	else
	  collect normalized-element into result
	finally (return
		  `(create-json-hash-table ,@result))))))

(set-macro-character +left-brace+ 'read-left-brace)

(defun reserve-character (character &optional (readtable *readtable*))
  "Reserve CHARACTER to make the reader consider the char it's own function."
  (declare (character character))
  (flet ((intern-character (stream char)
	   (declare (ignore stream char))
	   (intern (string character))))
    (set-macro-character character #'intern-character nil readtable)))

(defun read-separated-list (end-char &key
				       (input-stream *standard-input*)
				       (recursive-p nil))
  "Reads a list of elements from the INPUT-STREAM separated by the SEPARATOR.

Returns a list of lists, in which each sublist is the elements that were
separated by the SEPARTOR. If a trailing separator is found, a
JSON-COLLECTION-HAS-TRAILING-COMMA error is signaled."
  (declare (character end-char)
	   (stream)
	   (boolean recursive-p))
  (let ((*readtable* (copy-readtable)))
    (flet ((is-comma (element)
	     (and (symbolp element)
		  (eq element (intern (string +comma+))))))
      (reserve-character +comma+ *readtable*)
      (reserve-character +colon+ *readtable*)
      (set-macro-character end-char nil)
      (let ((elements
	      (read-delimited-list
	       end-char
	       input-stream
	       recursive-p)))
	(loop
	  with last-element = nil
	  with should-be-comma = nil
	  with elements = (consify-colons-on-list elements)
	  for element in elements
	  if (and should-be-comma (is-comma element))
	    do (setf should-be-comma nil)
	  else if (and should-be-comma (not (is-comma element)))
		 do (error 'json-elements-not-separated-by-comma
			   :object elements
			   :first-element last-element
			   :second-element element)
	  else if (and (not should-be-comma)
		       (is-comma element))
		 do (error 'json-unexpected-comma
			   :object elements
			   :after-element last-element)
          else
	    do (setf should-be-comma t)
	    and collect element into final-list
	    and do (setf last-element element)
	  finally (return
		    (if (and (not (null final-list))
			     (null should-be-comma))
			(error 'json-collection-has-trailing-comma
			       :object elements)
			final-list)))))))

(defun consify-colons-on-list (list)
  (declare (list list))
  (unless (null list)
    (flet ((wrap (element)
	     (cond ((null element) nil)
		   ((listp element) (list (consify-colons-on-list element)))
		   (t `(,element)))))
      (destructuring-bind (&optional a b c &rest rest) list
	(if (and (eq b (intern (string +colon+)))
		 (not (null c)))
	    `(`(cons ,,a ,,c) . ,(consify-colons-on-list rest))
	    (append (wrap a)
		    (consify-colons-on-list (append (wrap b) (wrap c) rest))))))))
