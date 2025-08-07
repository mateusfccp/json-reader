(in-package #:json-reader)

(defun read-delimited-list* (char &optional (stream *standard-input*) recursive-p)
  "Reads a list of objects from a stream, delimited by CHAR.

Unlike the standard `read-delimited-list`, this function peeks ahead for the
delimiter CHAR before attempting to read an object. This ensures the delimiter
is never consumed as part of a token. When CHAR is found, it is consumed from
the stream and the list of collected objects is returned."
  (declare (character char)
	   (stream stream)
	   (boolean recursive-p))
  (loop with output = (make-string-output-stream)
        until (and (eq (peek-char nil stream t nil recursive-p) char)
		   (read-char stream t nil recursive-p))
	do (write-char (read-char stream t nil recursive-p) output)
	finally (return
		  (with-input-from-string (stream (get-output-stream-string output))
			  (loop for item = (read stream nil :eof)
				until (eq item :eof)
				collect item)))))

(defun intern-character (stream char)
  "A SET-MACRO-CHARACTER compatible function to intern the received character."
  (declare (ignore stream))
  (intern (string char)))

(defun reserve-character (character &optional (readtable *readtable*))
  "Reserve CHARACTER to make the reader consider the char it's own function."
  (declare (character character))
  (set-macro-character character #'intern-character nil readtable))

