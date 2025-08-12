(in-package #:json-reader/tests)

(deftest read-delimited-list*
  (testing "Shouldn't read the delimiter when it's part of an element."
    (with-input-from-string (s "1 2 3 4]")
      (ok (equal (read-delimited-list* #\] s) '(1 2 3 4))
          "Simple list of numbers"))

    (with-input-from-string (s "hello \"world\" |symbol|]")
      (ok (equal (read-delimited-list* #\] s) '(hello "world" |symbol|))
          "Mixed data types"))

    (with-input-from-string (s "]")
      (ok (null (read-delimited-list* #\] s))
          "Empty list"))

    (with-input-from-string (s "(a b) (c (d)) e]")
      (ok (equal (read-delimited-list* #\] s) '((a b) (c (d)) e))
          "Nested list structures")))

  (testing "Should handle whitespaces and newlines correctly."
    (with-input-from-string (s "  1
                                2
                                  3 ]")
      (ok (equal (read-delimited-list* #\] s) '(1 2 3)))))

  (testing "Delimiter is NOT consumed as part of the last token."
    (let ((test-string "1 2 3] 4 5"))
      (with-input-from-string (s test-string)
        (let ((result-list (read-delimited-list* #\] s)))
          (ok (equal result-list '(1 2 3))
              "Should return the correct list")
          (ok (eql (read s) 4)
              "The next item in the stream should be available to read")))))

  (testing "Should throw END-OF-FILE error on unterminated list."
    (with-input-from-string (s "1 2 3")
      (ok (signals (read-delimited-list* #\] s) 'end-of-file))))

  (testing "Works with a different delimiter character"
    (let ((test-string "a b c; d e"))
      (with-input-from-string (s test-string)
        (let ((result-list (read-delimited-list* #\; s)))
          (ok (equal result-list '(a b c))
              "Should work with any specified delimiter char")
          (ok (eql (read s) 'd)
              "The next item should be readable after the list")))))

  (testing "Content after the last delimiter remains in the stream"
    (let ((test-string "a b ] c d ] e f"))
      (with-input-from-string (s test-string)
	(let ((result-list (read-delimited-list* #\] s)))
          (ok (equal result-list '(a b |]| c d))
              "Should parse the list up to the final delimiter."))

        (ok (eq (read s nil) 'e)
            "The first item after the last delimiter should be readable.")
        (ok (eq (read s nil) 'f)
            "The second item after the last delimiter should also be readable.")))))
