(prove:plan 6)

(prove:subtest
 "Test add-child"
 (prove:subtest
  "Test result"
  (defun test (f)
    (prove:is (funcall f '(1) 2) '(1 (2)))
    (prove:is (funcall f '(1) 2 3) '(1 (2) (3)))
    (prove:is (funcall f '(1) '(2)) '(1 (2)))
    (prove:is (funcall f '(1 (2)) '3) '(1 (2) (3)))
    (prove:is (funcall f '(1) (funcall f '(2) 3)) '(1 (2 (3))))
    (prove:is (funcall f '(1) nil) '(1 (nil)))
    (prove:is (funcall f '(1) `(,nil)) '(1 (nil))))
  (test #'add-child)
  (test #'insert-child))
 (prove:subtest
  "Test destructiveness"
  (defparameter *test-tree* '(1))
  (add-child *test-tree* 2)
  (prove:is *test-tree* '(1))
  (insert-child *test-tree* 2)
  (prove:is *test-tree* '(1 (2)))))


(prove:subtest
 "Test do-children"
 (defparameter *test-tree* (add-child '(1) 2 3 4))
 (defparameter *test-list* nil)
 (do-children (node *test-tree*)
   (setf *test-list* (cons (+ (get-node-value node) 1) *test-list*)))
 (prove:is *test-list* '(5 4 3)))

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

(prove:subtest "Test tree size and depth"
  (defparameter *test-tree*
    (add-child '(1) (add-child '(2) 3 4 5) 6))
  (prove:is (get-tree-size *test-tree*) 6)
  (prove:is (get-tree-depth *test-tree*) 2))

(prove:subtest "Test some getter funcs"
  (defparameter *test-tree* (add-child '(1) 2 3 4))
  (prove:is (get-node-value *test-tree*) 1)
  
  (prove:is (get-children *test-tree*) '((2) (3) (4)))
  (prove:is (get-children '(1)) nil)

  (prove:is (get-rest-children (get-children *test-tree*)) '((3) (4)))

  (prove:is (get-nth-child 0 *test-tree*) '(2))
  (prove:is (get-nth-child 1 *test-tree*) '(3))
  (prove:is (get-nth-child 2 *test-tree*) '(4))
  (prove:is (get-nth-child 3 *test-tree*) nil)
  (setf (car (get-nth-child 1 *test-tree*)) 5)
  (prove:is (get-nth-child 1 *test-tree*) '(5)))

(prove:subtest "Test select max funcs"
  (prove:is (select-max-child #'(lambda(val) (* val -1)) *test-tree*) '(2))
  
  (prove:is (select-max-node #'(lambda(val) (* val -1)) '(1 2 3)) 1))

(prove:finalize)
