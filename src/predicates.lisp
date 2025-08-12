(in-package #:json-reader)

(-> pairp (t) boolean)
(defun pairp (element)
  "Returns t if ELEMENT is a dotted pair.

A dotted pair is a CONS whose CDR is not a CONS."
  (and (consp element)
       (not (listp (cdr element)))))
