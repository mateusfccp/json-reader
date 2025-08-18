(in-package #:json-reader)

(-> assert-key-is-valid (t t) boolean)
(defun assert-key-is-valid (object key)
  "Throws an error if KEY is not a STRING."
  (unless (stringp key)
    (error 'invalid-json-key
	   :object object
	   :key key)))

(-> assert-value-is-valid (t t) boolean)
(defun assert-value-is-valid (object value)
  "Throws an error if VALUE is not a valid JSON value.

Valid JSON values are of type INTEGER, FLOAT, STRING, VECTOR, HASH-TABLE or the
symbols T, NIL and NULL."
  (unless (or (stringp value)
	      (integerp value)
	      (floatp value)
	      (vectorp value)
	      (hash-table-p value)
	      (eq t value)
	      (eq nil value)
	      (eq 'null value))
    (error 'invalid-json-value
	   :object object
	   :value value)))

(-> create-json-vector (&rest t) vector)
(defun create-json-vector (&rest elements)
  "Creates a VECTOR based on a LIST of elements or other LISTS.

If the item is a list, it will be spread into the vector."
  (loop
    for element in elements
    if (listp element)
      do (every (lambda (element)
		  (assert-value-is-valid elements element))
		element)
      and append element into result
    else
      do (assert-value-is-valid elements element)
      and collect element into result
    finally (return (coerce result 'vector))))

(-> create-json-hash-table (&rest t) hash-table)
(defun create-json-hash-table (&rest elements)
  "Creates a HASH-TABLE based on a LIST of pairs or hash tables.

If the item is a pair, a new key-value will be added to the HASH-TABLE based on
the pair value. If it is a HASH-TABLE, its contents will be merged into the one
being constructed. If it is a list, it will try to interpret this list as a
potential hash table my calling CREATE-JSON-HASH-TABLE on it and merging the
resulting HASH-TABLE into the original one."
  (loop
    with hash-table = (dict)
    for element in elements
    if (pairp element)
      do (let ((key (car element))
	       (value (cdr element)))
	   (assert-key-is-valid elements key)
	   (assert-value-is-valid elements value)
	   (setf (gethash key hash-table) value))
    else if (hash-table-p element)
	   do (do-hash-table (key value element)
		(assert-key-is-valid elements key)
		(assert-value-is-valid elements value))
	   and do (setf hash-table (merge-tables hash-table element))
    else if (listp element)
	   do (setf hash-table
		    (merge-tables hash-table
				  (apply #'create-json-hash-table element)))
    else do (error "Invalid type")
    finally (return hash-table)))

(-> consify-colons-on-list (list) list)
(defun consify-colons-on-list (list)
  (unless (null list)
    (flet ((wrap (element)
	     (cond ((null element) nil)
		   (t `(,element)))))
      (destructuring-bind (&optional a b c &rest rest) list
	(if (and (eq b (intern (string +colon+)))
		 (not (null c)))
	    `((cons ,a,c) . ,(consify-colons-on-list rest))
	    (append (wrap a)
		    (consify-colons-on-list (append (wrap b) (wrap c) rest))))))))

(-> normalize-json-value (t) t)
(defun normalize-json-value (value)
  "Normalize a JSON VALUE to a LISP equivalent."
  (cond
    ((symbolp value)
     (string-case (symbol-name value)
       ("TRUE" t)
       ("FALSE" ''nil)
       ("NULL" 'cl:null)
       (otherwise value)))
    (t value)))

(-> read-left-bracket (stream character))
(defun read-left-bracket (stream character)
  "Reads the left bracket character and parses the remaining STREAM with
READ-NEXT-OBJECT.

This will try to parse the given STREAM like a JSON list, and will return a
VECTOR with the internal elements."
  (declare (ignore character))
  (let ((elements (read-separated-list
		   +left-bracket+
		   +right-bracket+
		   :input-stream stream
		   :recursive-p t)))
    (loop
      for element in elements
      collect element into result
      finally
	 (return `(create-json-vector ,@result)))))

(-> read-left-brace (stream character))
(defun read-left-brace (stream character)
  "Reads the left brace character and parses the remaining STREAM with
READ-NEXT-OBJECT.

This will try to parse the given STREAM like a JSON object, and will return a
HASH-TABLE with the internal elements."
  (declare (ignore character))
  (let ((*readtable* (copy-readtable)))
    (reserve-character +colon+)
    (let ((elements (read-separated-list +left-brace+
					 +right-brace+
					 :input-stream stream
					 :recursive-p t)))
      (loop
	for element in elements
	collect element into result
	finally (return
		  `(create-json-hash-table ,@result))))))

(-> read-separeated-list (character
			  character
			  &key
			  (input-stream input-stream)
			  (recursive-p boolean))
    list)
(defun read-separated-list (opening-delimiter
			    closing-delimiter
			    &key
			      (input-stream *standard-input*)
			      (recursive-p nil))
  "Reads a list of elements from the INPUT-STREAM separated by the SEPARATOR.

Returns a list of lists, in which each sublist is the elements that were
separated by the SEPARTOR. If a trailing separator is found, a
JSON-COLLECTION-HAS-TRAILING-COMMA error is signaled."
  (let ((*readtable* (copy-readtable)))
    (flet ((is-comma (element)
	     (and (symbolp element)
		  (eq element (intern (string +comma+))))))
      (reserve-character +comma+ *readtable*)
      (let ((elements
	      (read-delimited-list*
	       opening-delimiter
	       closing-delimiter
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
	    and collect (normalize-json-value element) into final-list
	    and do (setf last-element element)
	  finally (return
		    (if (and (not (null final-list))
			     (null should-be-comma))
			(error 'json-collection-has-trailing-comma
			       :object elements)
			final-list)))))))
