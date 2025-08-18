(in-package #:json-reader)

(-> read-delimited-list* (character character &optional stream boolean) list)
(defun read-delimited-list* (opening-delimiter
			     closing-delimiter
			     &optional
			       (stream *standard-input*)
			       recursive-p)
  "Reads a list of objects from a stream up to the corresponding CLOSING-DELIMITER."
  (loop
    with nesting-level = 0
    with buffer = (make-string-output-stream)
    for character = (read-char stream t nil recursive-p)
    while (or (plusp nesting-level)
	      (char/= character closing-delimiter))
    do (progn
	 (cond ((char= character opening-delimiter)
		(incf nesting-level))
	       ((char= character closing-delimiter)
		(decf nesting-level)))
	 (write-char character buffer))
    finally
       ;; Parse the result-buffer if it contains anything.
       ;; This will only be the case if at least one closing-delimiter was found.
       (let ((content (get-output-stream-string buffer)))
         (if (plusp (length content))
             (with-input-from-string (stream content)
               (return
		 (loop for item = (read stream nil :eof)
		       until (eq item :eof)
		       collect item)))))))

(-> intern-character (stream character))
(defun intern-character (stream character)
  "A SET-MACRO-CHARACTER compatible function to intern the received character."
  (declare (ignore stream))
  (intern (string character)))

(-> reserve-character (character &optional readtable))
(defun reserve-character (character &optional (readtable *readtable*))
  "Reserve CHARACTER to make the reader consider the char it's own function."
  (set-macro-character character #'intern-character nil readtable))

(-> read-colon (stream character))
(defun read-colon (stream character)
  "Reads the colon character in a way that tries to allow it to be used as an
independent symbol but fallback to keyword when followed by other characters."
  (declare (ignore character))
  (let ((next-character (peek-char nil stream nil nil t)))
    (if (or (null next-character)
	    (whitespacep next-character))
	(progn
          (when next-character
	    (read-char stream))
          (intern ":"))
        (let ((*readtable*
		(if (and (boundp '*old-readtable*)
                         (readtablep *old-readtable*))
                    *old-readtable*
                    (copy-readtable nil))))
	  (unread-char +colon+ stream)
	  (read stream t nil t)))))

