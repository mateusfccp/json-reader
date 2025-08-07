(defpackage json-reader/tests
  (:use #:cl
	#:alexandria
	#:serapeum
	#:json-reader
	#:rove)
  (:import-from #:json-reader
		#:assert-key-is-valid
		#:assert-value-is-valid
		#:consify-colons-on-list
		#:create-json-hash-table
		#:create-json-vector
		#:invalid-json-key
		#:invalid-json-value
		#:json-collection-has-trailing-comma
		#:json-elements-not-separated-by-comma
		#:normalize-json-value
		#:pairp
		#:read-delimited-list*
		#:read-colon))

;; NOTE: To run the tests, execute `(asdf:test-system :json-reader)'.
