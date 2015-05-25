(prove:plan 6)

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
 "Test lazy evaluation"
 
 (prove:subtest
  "Test if it is evaluated only once"
  (defparameter *x* (lazy (princ "first") (+ 100 200)))
  (prove:is-print (force *x*) "first")
  (prove:is-print (force *x*) ""))
 
 (prove:subtest
  "Test if output is immutable"
  (defparameter *x* (lazy (print 'first) (+ 100 200)))
  (prove:is (force *x*) 300)
  (prove:is (force *x*) 300))

 (prove:subtest
  "Test lazy-car lazy-cdr"
  (defparameter *x* (lazy '(1 2 3)))
  (prove:is (lazy-car *x*) 1)
  (prove:is (lazy-cdr *x*) '(2 3)))

 (prove:subtest
  "Test lazy-setf-cdr"
  (defparameter *x* (lazy '(1 2 3)))
  (prove:is (lazy-setf-cdr *x*) '(2 3))
  (prove:is (force *x*) '(2 3))))

(prove:finalize)
