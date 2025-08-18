(in-package #:json-reader/tests)

(deftest read-delimited-list*
  (testing "Shouldn't read the delimiter when it's part of an element."
    (with-input-from-string (stream "1 2 3 4]")
      (ok (equal (read-delimited-list* #\[ #\] stream) '(1 2 3 4))
          "Simple list of numbers"))
    (with-input-from-string (stream "1 2 3 4]")
      (ok (equal (read-delimited-list* #\[ #\] stream) '(1 2 3 4))
          "Simple list of numbers"))

    (with-input-from-string (stream "hello \"world\" |symbol|]")
      (ok (equal (read-delimited-list* #\[ #\] stream) '(hello "world" |symbol|))
          "Mixed data types"))

    (with-input-from-string (stream "]")
      (ok (null (read-delimited-list* #\[ #\] stream))
          "Empty list"))

    (with-input-from-string (stream "(a b) (c (d)) e]")
      (ok (equal (read-delimited-list* #\[ #\] stream) '((a b) (c (d)) e))
          "Nested list structures")))

  (testing "Should handle whitespaces and newlines correctly."
    (with-input-from-string (stream "  1
                                2
                                  3 ]")
      (ok (equal (read-delimited-list* #\[ #\] stream) '(1 2 3)))))

  (testing "Delimiter is NOT consumed as part of the last token."
    (let ((test-string "1 2 3] 4 5"))
      (with-input-from-string (stream test-string)
        (let ((result-list (read-delimited-list* #\[ #\] stream)))
          (ok (equal result-list '(1 2 3))
              "Should return the correct list")
          (ok (eql (read stream) 4)
              "The next item in the stream should be available to read")))))

  (testing "Should throw END-OF-FILE error on unterminated list."
    (with-input-from-string (stream "1 2 3")
      (ok (signals (read-delimited-list* #\[ #\] stream) 'end-of-file))))

  (testing "Works with a different delimiter character"
    (let ((test-string "a b c} d e"))
      (with-input-from-string (stream test-string)
        (let ((result-list (read-delimited-list* #\{ #\} stream)))
          (ok (equal result-list '(a b c))
              "Should work with any specified delimiter char")
          (ok (eql (read stream) 'd)
              "The next item should be readable after the list")))))

  (testing "Content after the matching delimiter remains in the stream"
    (let ((test-string "a b ] c d ] e f"))
      (with-input-from-string (stream test-string)
	(let ((result-list (read-delimited-list* #\[ #\] stream)))
          (ok (equal result-list '(a b))
              "Should parse the list up to the final delimiter."))

	(let ((remaining (read-line stream)))
          (ok (equal remaining " c d ] e f")))))))
