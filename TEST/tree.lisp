(prove:plan 4)

(prove:subtest "Test add-child"
  (prove:is (add-child '(1) 2) '(1 (2)))
  (prove:is (add-child '(1) '(2)) '(1 (2)))
  (prove:is (add-child '(1 (2)) '3) '(1 (2) (3)))
  (prove:is (add-child '(1) (add-child '(2) 3)) '(1 (2 (3))))
  (prove:is (add-child '(1) nil) '(1 (nil)))
  (prove:is (add-child '(1) `(,nil)) '(1 (nil))))


(prove:subtest "Test get-num-child"
  (defparameter *test-tree* '(1))
  (prove:ok (not (has-children *test-tree*)))
  (prove:is (get-num-children *test-tree*) 0)

  (setf *test-tree* (add-child *test-tree* 2))
  (prove:ok (has-children *test-tree*))
  (prove:is (get-num-children *test-tree*) 1)
  
  (setf *test-tree* (add-child *test-tree* 3))
  (prove:ok (has-children *test-tree*))
  (prove:is (get-num-children *test-tree*) 2))


(prove:subtest "Test some getter funcs"
  (prove:is (get-node-value *test-tree*) 1)
  
  (prove:is (get-children *test-tree*) '((2) (3)))
  (prove:is (get-children '(1)) nil)

  (prove:is (get-nth-child 0 *test-tree*) '(2))
  (prove:is (get-nth-child 1 *test-tree*) '(3))
  (prove:is (get-nth-child 2 *test-tree*) nil)
  (setf (car (get-nth-child 1 *test-tree*)) 5)
  (prove:is (get-nth-child 1 *test-tree*) '(5))
  )

(prove:subtest "Test select max funcs"
  (prove:is (select-max-child #'(lambda(val) (* val -1)) *test-tree*) '(2))
  
  (prove:is (select-max-node #'(lambda(val) (* val -1)) '(1 2 3)) 1))

(prove:finalize)
