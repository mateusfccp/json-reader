(in-package #:json-reader/tests)

(defparameter *object* (make-hash-table :test 'equal))

(defhook clear-object-hook :before
  (clrhash *object*))

(deftest json-data-validation
  (testing "Only strings should be accept as JSON keys."
    (ok (signals (assert-key-is-valid :object nil)
	    'invalid-json-key)
	"The key can't be a nil")
    (ok (signals (assert-key-is-valid :object 0)
	    'invalid-json-key)
	"The key can't be a integer")
    (ok (signals (assert-key-is-valid :object 0.0)
	    'invalid-json-key)
	"The key can't be a double")
    (ok (signals (assert-key-is-valid :object :keyword)
	    'invalid-json-key)
	"The key can't be a keyword")
    (ok (signals (assert-key-is-valid :object 'symbol)
	    'invalid-json-key)
	"The key can't be a symbol")
    (ok (signals (assert-key-is-valid :object #'eq)
	    'invalid-json-key)
	"The key can't be a function")
    (ok (signals (assert-key-is-valid :object (list 0))
	    'invalid-json-key)
	"The key can't be a list")
    (ok (signals (assert-key-is-valid :object *object*)
	    'invalid-json-key)
	"The key can't be a hash table")
    (ok (signals (assert-key-is-valid :object #())
	    'invalid-json-key)
	"The key can't be a vector")
    (ng (signals (assert-key-is-valid :object "string"))
	"The key must be a string"))

  (testing "Only numbers, strings, true, false, null, lists and objects should be accept as JSON value."
    (ng (signals (assert-value-is-valid :object nil))
	"The value can be nil")
    (ng (signals (assert-value-is-valid :object 0))
	"The value can be an integer")
    (ng (signals (assert-value-is-valid :object 0.0))
	"The value can be a double")
    (ok (signals (assert-value-is-valid :object :keyword)
	    'invalid-json-value)
	"The value can't be a keyword")
    (ok (signals (assert-value-is-valid :object 'symbol)
	    'invalid-json-value)
	"The value can't be any symbol")
    (ok (signals (assert-value-is-valid :object #'eq)
	    'invalid-json-value)
	"The value can't be a function")
    (ok (signals (assert-value-is-valid :object '(1 2 3))
	    'invalid-json-value)
	"The value can't be a list")
    (ng (signals (assert-value-is-valid :object *object*))
	"The value can be a hash table")
    (ng (signals (assert-value-is-valid :object #()))
	"The value can be a vector")
    (ng (signals (assert-value-is-valid :object "string key"))
	"The value can be a string")
    (ng (signals (assert-value-is-valid :object 'null))
	"The value can be the 'null symbol")))

(deftest create-json-vector
  (testing "An empty list should result in an empty vector."
    (let ((vector (create-json-vector)))
      (ok (equalp vector #()))))

  (testing "An list of elements should result in a vector with the corresponding elements."
    (let ((vector (create-json-vector 1 2 3)))
      (ok (equalp vector #(1 2 3)))))

  (testing "An list as an element of the main list should be streamlined into the main list."
    (let ((vector (create-json-vector 1 2 3 '(4 5) 6)))
      (ok (equalp vector #(1 2 3 4 5 6)))))

  (testing "The elements of the list should only be valid JSON values."
    (ok (signals (create-json-vector #'eq)
	    'invalid-json-value))))

(deftest create-json-hash-table
  (testing "An empty list should result in an empty hash table."
    (let ((hash-table (create-json-hash-table))
	  (expected *object*))
      (ok (equalp hash-table expected))))

  (testing "A list of pairs should generate an equivalent hash table."
    (let ((hash-table (create-json-hash-table '("foo" . "bar") '("baz" . "qux")))
	  (expected *object*))
      (setf (gethash "foo" expected) "bar")
      (setf (gethash "baz" expected) "qux")
      (ok (equalp hash-table expected))))

  (testing "An hash table as an element should be streamlined into the main hash table."
    (let ((hash-table (create-json-hash-table '("foo" . "bar") (dict "baz" "qux")))
	  (expected *object*))
      (setf (gethash "foo" expected) "bar")
      (setf (gethash "baz" expected) "qux")
      (ok (equalp hash-table expected))))

  (testing "The keys of the hash table should be valid JSON keys."
    (ok (signals (create-json-hash-table '(0 . "bar"))
	    'invalid-json-key))
    (ok (signals (create-json-hash-table '(0.0 . "bar"))
	    'invalid-json-key))
    (ok (signals (create-json-hash-table '(nil . "bar"))
	    'invalid-json-key))
    (ok (signals (create-json-hash-table '(#'eq . "bar"))
	    'invalid-json-key))
    (ok (signals (create-json-hash-table '(#() . "bar"))
	    'invalid-json-key))
    (ok (signals (create-json-hash-table '(*object* . "bar"))
	    'invalid-json-key))
    (ok (signals (create-json-hash-table '(t . "bar"))
	    'invalid-json-key))
    (ok (signals (create-json-hash-table '('null . "bar"))
	    'invalid-json-key))
    (ng (signals (create-json-hash-table '("foo" . "bar")))))

  (testing "The values of the hash table should be valid JSON values."
    (ng (signals (create-json-hash-table '("foo" . 0))))
    (ng (signals (create-json-hash-table '("foo" . 0.0))))
    (ng (signals (create-json-hash-table '("foo" . nil))))
    (ok (signals (create-json-hash-table `("foo" . ,#'eq))
	    'invalid-json-value))
    (ng (signals (create-json-hash-table '("foo" . #()))))
    (ng (signals (create-json-hash-table `("foo" . ,*object*))))
    (ng (signals (create-json-hash-table '("foo" . t))))
    (ng (signals (create-json-hash-table '("foo" . null))))
    (ng (signals (create-json-hash-table '("foo" . "bar"))))))

(deftest pairp
  (testing "A pair is a cons cell with a car and a non-list cdr."
    (ok (pairp '("foo" . "bar")))
    (ng (pairp '("foo" . ("bar" . "baz"))))
    (ng (pairp nil))
    (ng (pairp '("foo" "bar")))))

(deftest json-to-lisp-normalization
  (testing "Non-symbols shouldn't be normalized."
    (ok (eq (normalize-json-value *object*) *object*))
    (ok (eq (normalize-json-value 0) 0))
    (ok (eq (normalize-json-value 0.0) 0.0))
    (ok (eq (normalize-json-value "foo") "foo"))
    (ok (eq (normalize-json-value nil) nil))
    (ok (eq (normalize-json-value #'eq) #'eq)))

  (testing "The 'true symbol should be normalized to t."
    (ok (eq (normalize-json-value 'true) t)))

  (testing "The 'false symbol should be normalized to nil."
    (ok (eq (normalize-json-value 'false) nil)))

  (testing "The 'null symbol should be normalized to 'null."
    (ok (eq (normalize-json-value 'null) 'null)))

  (testing "Other symbols should not be normalized."
    (ok (eq (normalize-json-value 'foo) 'foo))
    (ok (eq (normalize-json-value :foo) :foo))))

(deftest consify-colons-on-list
  (testing "Each two elements separated by a colon symbol in a list should be transformed into a cons pair."
    (ok (equalp (consify-colons-on-list '("foo" |:| "bar" "baz" |:| "qux"))
		'((cons "foo" "bar") (cons "baz" "qux"))))))

(deftest colon-reader
  (json-reader-enable)

  (testing "A colon should be read as it's own token when it is followed by a Common Lisp whitespace."
    (ok (eq (read-from-string ": ") '|:|)))

  (testing "A colon should be read as it's own token when it is followed by a Common Lisp newline."
    (ok (eq (read-from-string (format nil ":~%")) '|:|)))

  (testing "A colon should be read as a keyword when followed by a symbol."
    (ok (eq (read-from-string ":foo") :foo)))

  (testing "A standalone colon should be read as it's own token."
    (ok (eq (read-from-string ":" nil :eof) '|:|)))

  (json-reader-disable))

(defmacro with-json-reader (binding expression &body forms)
  `(let* ((result (read-from-string ,expression))
	  (,binding (eval result)))
     ,@forms))

(deftest json-reader-syntax
  (json-reader-enable)
  
  (testing "A list should be read as a vector."
    (with-json-reader vector "[]"
      (ok (vectorp vector))))

  (testing "An empty list should be read as an empty vector."
    (with-json-reader vector "[]"
      (ok (length= 0 vector))))

  (testing "A list should generate a vector with its contents."
    (with-json-reader vector "[1, 2, 3]"
      (ok (equalp vector #(1 2 3)))))

  (testing "A list shouldn't have a trailing comma."
    (ok (signals (read-from-string "[1, 2, 3,]")
	    'json-collection-has-trailing-comma)))

  (testing "All elements in a list should be separated by a comma."
    (ok (signals (read-from-string "[1, 2 3]")
	    'json-elements-not-separated-by-comma)))

  (testing "Using non-JSON elements should be an error."
    (ok (signals (eval (read-from-string "[1, 2, 'eq]"))
	    'invalid-json-value)
	"Quoted symbol")
    (ok (signals (eval (read-from-string "[1, 2, #'eq]"))
	    'invalid-json-value)
	"Function")
    (ok (signals (eval (read-from-string "[1, 2, :foo]"))
	    'invalid-json-value)
	"Keyword"))
  
  (testing "An object should be read as a hash table."
    (with-json-reader hash-table "{}"
      (ok (hash-table-p hash-table))))

  (testing "An empty object `{}` should be read as an empty hashtable."
    (with-json-reader hash-table "{}"
      (ok (equal (hash-table-count hash-table) 0))))

  (testing "An object TEST slot should be EQUAL."
    (with-json-reader hash-table "{}"
      (ok (eq (hash-table-test hash-table) 'equal))))

  (json-reader-disable))
