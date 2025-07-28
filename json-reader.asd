(defsystem "json-reader"
  :version "0.0.1"
  :author "Mateus Felipe Cordeiro Caetano Pinto"
  :mailto "mateusfccp@gmail.com"
  :homepage "https://github.com/mateusfccp/json-reader"
  :license "MIT"
  :depends-on (#:alexandria #:serapeum)
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "globals")
		 (:file "conditions")
		 (:file "constants")
		 (:file "core")
		 (:file "main"))))
  :description "A reader macro to read JSON within Common Lisp."
  :in-order-to ((test-op (test-op "json-reader/tests"))))

(defsystem "json-reader/tests"
  :author "Mateus Felipe Cordeiro Caetano Pinto"
  :license "MIT"
  :depends-on ("json-reader"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for json-reader"
  :perform (test-op (op c) (symbol-call :rove :run c)))
