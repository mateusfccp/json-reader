(in-package #:json-reader)

(define-condition json-parse-error (error)
  ((object :initarg :object))
  (:documentation "The base error for JSON parsing."))

(define-condition json-collection-has-trailing-comma (json-parse-error) ()
  (:documentation "An error indicating that an unwanted trailing comma was
found when parsing a JSON expression.")
  (:report report-json-collection-has-trailing-comma))

(defun report-json-collection-has-trailing-comma (condition stream)
  (declare (error condition)
	   (stream stream))
  (with-slots (object first-element second-element) condition
    (write-line "Trailing comma found while parsing JSON." stream)
    (write-line "" stream)
    (write-line (format nil "In object: ~A" object) stream)))

(define-condition json-elements-not-separated-by-comma (json-parse-error)
  ((first-element :initarg :first-element)
   (second-element :initarg :second-element))
  (:documentation "An error indicating that there are two elements in a JSON
expression that are not spearated by a comma.")
  (:report report-json-elements-not-separated-by-comma))

(defun report-json-elements-not-separated-by-comma (condition stream)
  (declare (error condition)
	   (stream stream))
  (with-slots (object first-element second-element) condition
    (write-line "Two sequential elements not separated by comma found while parsing JSON." stream)
    (write-line "" stream)
    (write-line (format nil "In object: ~A" object) stream)
    (write-line "" stream)
    (write-line (format nil "First element: ~A" first-element) stream)
    (write-line (format nil "Second element: ~A" second-element) stream)))

(define-condition json-unexpected-comma (json-parse-error)
  ((after-element :initarg :after-element))
  (:documentation "An error indicating that there is a comma in an unexpexted
place in the JSON expression. ")
  (:report report-json-unexpected-comma))

(defun report-json-unexpected-comma (condition stream)
  (declare (error condition)
	   (stream stream))
  (with-slots (object after-element) condition
    (write-line "Unexpexted comma found while parsing JSON." stream)
    (write-line "" stream)
    (write-line (format nil "In object: ~A" object) stream)
    (write-line "" stream)
    (if (null after-element)
	(write-line "At the beginning of the object." stream)
	(write-line (format nil "After element: ~A" after-element) stream))
    (write-line "" stream)
    (write-line "Expected a JSON object or delimiter." stream)))
