(in-package #:json-reader)

(-> read-delimited-list* (character &optional stream boolean) list)
(defun read-delimited-list* (delimiter &optional
					 (stream *standard-input*)
					 recursive-p)
  "Reads a list of objects from a stream up to the LAST occurrence of DELIMITER.

If DELIMITER is found, the function returns the list of objects read before that
final delimiter. All characters after the final delimiter are pushed back onto
the stream.

If no delimiter is found before the end of the stream, an `end-of-file` error is
signaled."
  (loop
    with result-buffer = (make-string-output-stream)
    with pending-buffer = (make-string-output-stream)
    for character = (read-char stream nil nil recursive-p)
    while character
    do (if (char= character delimiter)
           (progn
             (write-string (get-output-stream-string pending-buffer) result-buffer)
             (write-char character result-buffer)
             (setf pending-buffer (make-string-output-stream)))
           (write-char character pending-buffer))
    finally
       ;; Unread pending characters so they can be used afterwards.
       (loop
	 for character across (reverse (get-output-stream-string pending-buffer))
         do (unread-char character stream))

       ;; Parse the result-buffer if it contains anything.
       ;; This will only be the case if at least one delimiter was found.
       (let ((result-string (get-output-stream-string result-buffer)))
         (if (plusp (length result-string))
           (let ((string-to-parse (subseq result-string 0 (1- (length result-string)))))
             (when (plusp (length string-to-parse))
               (with-input-from-string (s string-to-parse)
                 (return
		   (loop for item = (read s nil :eof)
			 until (eq item :eof)
			 collect item)))))
	   (error 'end-of-file :stream stream)))))

(-> intern-character (stream character))
(defun intern-character (stream character)
  "A SET-MACRO-CHARACTER compatible function to intern the received character."
  (declare (ignore stream))
  (intern (string character)))

(-> reserve-character (character &optional readtable))
(defun reserve-character (character &optional (readtable *readtable*))
  "Reserve CHARACTER to make the reader consider the char it's own function."
  (set-macro-character character #'intern-character nil readtable))

