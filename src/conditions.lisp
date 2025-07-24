(in-package #:json-reader)

(define-condition json-collection-has-trailing-comma (error) ()
  (:documentation "An error indicating that an unwanted trailing comma was
found when parsing a JSON expression."))

(define-condition json-elements-not-separated-by-comma (error)
  ((first-element :initarg :first-element)
   (second-element :initarg :second-element))
  (:documentation "An error indicating that there are two elements in a JSON
expression that are not spearated by a comma.")
  (:report report-json-elements-not-separated-by-comma))

(defun report-json-elements-not-separated-by-comma (condition stream)
  (declare (error condition)
	   (stream stream))
  (let ((first-element (slot-value condition 'first-element))
	(second-element (slot-value condition 'second-element)))
    (write-line "Two sequential elements not separated by comma found." stream)
    (write-line "" stream)
    (write-line "First element:" stream)
    (write-line "" stream)
    (write-line (format nil "~4T~A" first-element) stream)
    (write-line "" stream)
    (write-line "Second element:" stream)
    (write-line "" stream)
    (write-line (format nil "~4T~A" second-element) stream)
    (write-line "" stream)))

(define-condition json-sequential-commas (error) ()
  (:documentation "An error indicating that there are two commas in a JSON
expression are not separated by an object. "))
