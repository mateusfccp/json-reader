(in-package #:json-reader/tests)

(defparameter *object* (make-hash-table :test 'equal))

(defhook clear-object-hook :before
  (clrhash *object*))

(deftest json-data-validation
  (testing "Only strings should be accept as JSON keys."
    (ok (signals (json-reader::assert-key-is-valid :object nil)
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::assert-key-is-valid :object 0)
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::assert-key-is-valid :object 0.0)
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::assert-key-is-valid :object :keyword)
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::assert-key-is-valid :object 'symbol)
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::assert-key-is-valid :object #'eq)
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::assert-key-is-valid :object (list))
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::assert-key-is-valid :object *object*)
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::assert-key-is-valid :object #())
	    'json-reader::invalid-json-key))
    (ng (signals (json-reader::assert-key-is-valid :object "string key"))))

  (testing "Only numbers, strings, true, false, null, lists and objects should be accept as JSON value."
    (ng (signals (json-reader::assert-value-is-valid :object nil)))
    (ng (signals (json-reader::assert-value-is-valid :object 0)))
    (ng (signals (json-reader::assert-value-is-valid :object 0.0)))
    (ok (signals (json-reader::assert-value-is-valid :object :keyword)
	    'json-reader::invalid-json-value))
    (ok (signals (json-reader::assert-value-is-valid :object 'symbol)
	    'json-reader::invalid-json-value))
    (ok (signals (json-reader::assert-value-is-valid :object #'eq)
	    'json-reader::invalid-json-value))
    (ok (signals (json-reader::assert-value-is-valid :object '(1 2 3))
	    'json-reader::invalid-json-value))
    (ng (signals (json-reader::assert-value-is-valid :object *object*)))
    (ng (signals (json-reader::assert-value-is-valid :object #())))
    (ng (signals (json-reader::assert-value-is-valid :object "string key")))
    (ng (signals (json-reader::assert-value-is-valid :object 'null)))))

(deftest create-json-vector
  (testing "An empty list should result in an empty vector."
    (let ((vector (json-reader::create-json-vector)))
      (ok (equalp vector #()))))

  (testing "An list of elements should result in a vector with the corresponding elements."
    (let ((vector (json-reader::create-json-vector 1 2 3)))
      (ok (equalp vector #(1 2 3)))))

  (testing "An list as an element of the main list should be streamlined into the main list."
    (let ((vector (json-reader::create-json-vector 1 2 3 '(4 5) 6)))
      (ok (equalp vector #(1 2 3 4 5 6)))))

  (testing "The elements of the list should only be valid JSON values."
    (ok (signals (json-reader::create-json-vector #'eq)
	    'json-reader::invalid-json-value))))

(deftest create-json-hash-table
  (testing "An empty list should result in an empty hash table."
    (let ((hash-table (json-reader::create-json-hash-table))
	  (expected *object*))
      (ok (equalp hash-table expected))))

  (testing "A list of pairs should generate an equivalent hash table."
    (let ((hash-table (json-reader::create-json-hash-table '("foo" . "bar") '("baz" . "qux")))
	  (expected *object*))
      (setf (gethash "foo" expected) "bar")
      (setf (gethash "baz" expected) "qux")
      (ok (equalp hash-table expected))))

  (testing "An hash table as an element should be streamlined into the main hash table."
    (let ((hash-table (json-reader::create-json-hash-table '("foo" . "bar") (dict "baz" "qux")))
      (expected *object*))
    (setf (gethash "foo" expected) "bar")
    (setf (gethash "baz" expected) "qux")
    (ok (equalp hash-table expected))))

  (testing "The keys of the hash table should be valid JSON keys."
    (ok (signals (json-reader::create-json-hash-table '(0 . "bar"))
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::create-json-hash-table '(0.0 . "bar"))
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::create-json-hash-table '(nil . "bar"))
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::create-json-hash-table '(#'eq . "bar"))
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::create-json-hash-table '(#() . "bar"))
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::create-json-hash-table '(*object* . "bar"))
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::create-json-hash-table '(t . "bar"))
	    'json-reader::invalid-json-key))
    (ok (signals (json-reader::create-json-hash-table '('null . "bar"))
	    'json-reader::invalid-json-key))
    (ng (signals (json-reader::create-json-hash-table '("foo" . "bar")))))

  (testing "The values of the hash table should be valid JSON values."
    (ng (signals (json-reader::create-json-hash-table '("foo" . 0))))
    (ng (signals (json-reader::create-json-hash-table '("foo" . 0.0))))
    (ng (signals (json-reader::create-json-hash-table '("foo" . nil))))
    (ok (signals (json-reader::create-json-hash-table `("foo" . ,#'eq))
	    'json-reader::invalid-json-value))
    (ng (signals (json-reader::create-json-hash-table '("foo" . #()))))
    (ng (signals (json-reader::create-json-hash-table `("foo" . ,*object*))))
    (ng (signals (json-reader::create-json-hash-table '("foo" . t))))
    (ng (signals (json-reader::create-json-hash-table '("foo" . null))))
    (ng (signals (json-reader::create-json-hash-table '("foo" . "bar"))))))

(defmacro with-json-list (binding text &body forms)
  (declare (symbol binding)
	   (string text))
  (with-gensyms (stream)
    `(with-input-from-string (,stream ,text)
       (let ((,binding (read-left-bracket ,stream +left-bracket+)))
	 ,@forms))))

(deftest json-reader-syntax
  ;; (testing "A list should be read as a vector."
  ;;   (with-json-list (vector "[]")
  ;;     (ok (vectorp vector))))

  ;; (testing "An empty list should be read as an empty vector."
  ;;   (ok (length= 0 [])))

  ;; (testing "A list should generate a vector with its contents."
  ;;   (let ((list [1, 2, 3]))
  ;;     (ok (equalp list #(1 2 3)))))

  ;; (testing "A list shouldn't have a trailing comma."
  ;;   (ok (signals (list [1, 2, 3,])
  ;; 	    'json-reader::json-collection-has-trailing-comma)))
  
  ;; (testing "An object should be read as a hashtable."
  ;;   (let ((hashtable {}))
  ;;     (ok (hash-table-p hashtable))))

  ;; (testing "An empty object `{}` should be read as an empty hashtable."
  ;;   (let ((hashtable {}))
  ;;     (ok (equal (hash-table-count hashtable) 0))))

  ;; (testing "An object TEST slot should be EQUAL."
  ;;   (let ((hashtable {}))
  ;;     (ok (eq (hash-table-test hashtable) 'equal))))
  )
