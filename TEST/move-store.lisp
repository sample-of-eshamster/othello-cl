(prove:plan 7)

(load "TEST/test-util.lisp")

(prove:subtest
    "Test init-move-store"
  (let ((store (init-move-store)))
    (prove:is-type store 'move-store)
    (prove:is (move-store-count store) 0)
    (prove:ok (> (length (move-store-moves store))0))
    (let ((result t))
      (dotimes (i (length (move-store-moves store)))
	   (unless (move-p (aref (move-store-moves store) i))
	     (print (aref (move-store-moves store) i))
	     (setf result nil)
	     (return)))
      (prove:ok result "All elements are the move structure"))))

(prove:subtest
    "Test add, get and reset move-store"
  (let ((store (init-move-store)))
    (prove:subtest "Test adding from init"
      (prove:is-type (add-to-move-store store 1 2) 'move-store)
      (prove:is-type (add-to-move-store store 3 4) 'move-store)
      (prove:is (move-store-count store) 2)
      (prove:is (get-nth-move store 0) (make-a-move 1 2) :test #'equalp)
      (prove:is (get-nth-move store 1) (make-a-move 3 4) :test #'equalp))
    (prove:subtest "Test error paths of getting"
      (prove:ok (not (get-nth-move nil 0)))
      (prove:ok (not (get-nth-move store -1)))
      (prove:ok (not (get-nth-move store 2))))
    (prove:subtest "Test resetting and re-adding"
      (prove:is-type (reset-move-store store) 'move-store)
      (prove:is (move-store-count store) 0)
      (add-to-move-store store 5 6)
      (prove:is (move-store-count store) 1)
      (prove:is (get-nth-move store 0) (make-a-move 5 6)))))

(prove:subtest
    "Test do-move-store and contains-move"
  (let ((store (init-move-store)))
    (add-to-move-store store 1 2)
    (add-to-move-store store 3 4)
    (prove:ok (contains-move store 1 2))
    (prove:ok (contains-move store 3 4))
    (prove:ok (not (contains-move store 1 10)))))

(prove:subtest
    "Test copy and clone move-store"
  (labels ((test-isnt-same (copied org)
	     (let ((len (move-store-count org)))
	       (prove:isnt org copied)
	       (add-to-move-store org 5 6)
	       (prove:is (move-store-count copied) len)
	       (prove:is (move-store-count org) (1+ len)))))
    (let* ((store (init-move-store))
	   (deep-copy (init-move-store))
	   (shallow-copy store))
      (prove:is store shallow-copy)
      (add-to-move-store store 1 2)
      (add-to-move-store store 3 4)
      
      (prove:is-type (copy-move-store deep-copy store) 'move-store)
      (test-isnt-same deep-copy store)

      (prove:is-type (clone-move-store store) 'move-store)
      (test-isnt-same (clone-move-store store) store))))

(prove:subtest
    "Test move-store-stack"
  (labels ((test-stack-num (stack allocated reserved)
	     (prove:is (num-allocated-move-store-stack stack) allocated)
	     (prove:is (num-reserved-move-store-stack stack) reserved)))
    (let ((store (init-move-store))
	  (stack (make-move-store-stack)))
      (test-stack-num stack 0 0)
      (add-to-move-store store 1 2)
      (add-to-move-store store 3 4)
      
      (with-cloned-move-store stack (clone1 store)
	(test-stack-num stack 1 1)
	(add-to-move-store clone1 5 6)
	(with-cloned-move-store stack (clone2 store)
	  (test-stack-num stack 2 2)
	  (add-to-move-store clone2 2 1)
	  (add-to-move-store clone2 4 3)
	  (prove:is (move-store-count clone2) 4))
	(test-stack-num stack 2 1)
	(prove:is (move-store-count clone1) 3))
      
      (test-stack-num stack 2 0)
      (prove:is (move-store-count store) 2)
      
      (with-cloned-move-store stack (clone3 store)
	(test-stack-num stack 2 1)))))

(prove:subtest
    "Test get-nth-move"
  (let ((store (init-move-store)))
    (add-to-move-store store 1 2)
    (add-to-move-store store 3 4)
    
    (prove:ok (not (get-nth-move store -1)))
    (prove:ok (not (get-nth-move store 10)))
    (prove:ok (not (get-nth-move nil 0)))
    (prove:is (get-nth-move store 1) (make-a-move 3 4))))

(prove:subtest
    "Test mapcar-move-store"
  (let* ((store (make-moves (make-nth-test-game 5)))
	 (move-list (mapcar-move-store #'(lambda (move) move) store)))
    (prove:is (move-store-count store) (length move-list))
    (dolist (move move-list)
      (prove:ok (contains-move store (move-x move) (move-y move))))))

(prove:finalize)
