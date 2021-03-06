(prove:plan 7)

(prove:subtest
    "Test string-to-list"
  (prove:is (string-to-list "test") '(TEST))
  (prove:is (string-to-list "ThiS is a test") '(THIS IS A TEST)))

(prove:subtest
    "Test stream-to-list"
  (labels ((test (com list)
	     (prove:is (stream-to-list (make-string-input-stream com)) list)))
    (test "test" '(TEST))
    (test "thIs is a Test" '(THIS IS A TEST))))

(prove:subtest
    "Test to-string"
  (prove:is (to-string "test") "test")
  (prove:is (to-string 123) "123")
  (prove:is (to-string #'+) "+")
  (prove:is (to-string 'test) "TEST"))

(prove:subtest
    "Test concat-symbol"
  (prove:is (concat-symbol) nil)
  (prove:is (concat-symbol 'abc) 'abc)
  (prove:is (concat-symbol 'abc- 'def) 'abc-def)
  (prove:is (concat-symbol 'abc- 'def 'gh) 'abc-defgh))

(prove:subtest
    "Test push-without-dup"
  (prove:is (push-without-dup 3 '(1 2) #'=) '(1 2 3))
  (prove:is (push-without-dup 1 '(1 2) #'=) '(1 2))
  (prove:is (push-without-dup '(3 6) '((1 2) (2 4))
			      #'(lambda (a b) (= (car a) (car b))))
	    '((1 2) (2 4) (3 6)))
  (prove:is (push-without-dup '(2 6) '((1 2) (2 4) (2 5) (3 6))
			      #'(lambda (a b) (= (car a) (car b))))
	    '((1 2) (2 6) (2 5) (3 6))))

(prove:subtest
    "Test read-line-while"
  (prove:is (read-line-while "test"
			     #'(lambda (str)
				 (not (equal str "test")))
			     (make-string-input-stream
			      (format nil "   ~%abc~%~%  test "))) "test"))

(prove:subtest
    "Test aif-second-true"
  (prove:is (aif-second-true (values 1 t) (+ it 10) 1234)
	    11)
  (prove:is (aif-second-true (values 1 nil) (+ it 10) 1234)
	    1234))

(prove:finalize)
