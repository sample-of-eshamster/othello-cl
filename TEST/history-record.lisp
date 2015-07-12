(prove:plan 2)

(load "TEST/test-util.lisp")

(prove:subtest
    "Test history-record"
  (prove:subtest
      "Test make"
    (let ((record (make-history-record)))
      (prove:ok (move-p (history-record-move record)))
      (prove:ok (move-store-p (history-record-reverse-list record)))
      (prove:is (array-total-size (move-store-moves (history-record-reverse-list record)))
		*max-reverse-list*))))

(prove:subtest
    "Test history-record-store"
  (labels ((test-count (store expected)
	     (prove:is (history-record-store-count store) expected))
	   (test-record (records record-idx move-idx expected-x expected-y)
	     (prove:is (get-nth-move (history-record-reverse-list
				      (aref records record-idx))
				     move-idx)
		       (make-a-move expected-x expected-y)
		       :test #'equalp))
	   (make-fn-add-move (x y)
	     #'(lambda (record)
		 (add-to-move-store (history-record-reverse-list record) x y))))
    (prove:subtest
	"Test make"
      (let* ((store (make-history-record-store))
	     (records (history-record-store-records store)))
	(test-count store 0)
	(prove:is (array-total-size records) *max-history-record*)
	(let ((result t))
	  (dotimes (i *max-history-record*)
	    (when (not (history-record-p (aref records i)))
	      (setf result nil)
	      (return)))
	  (prove:ok result "All elements are history-record-p"))))
    
    (prove:subtest
	"Test regist and pop"
      (let* ((store (make-history-record-store))
	     (records (history-record-store-records store)))
	(prove:subtest
	    "Test not regist"
	  (regist-new-history-record store 
				     #'(lambda (record) nil))
	  (test-count store 0))
	
	(prove:subtest
	    "Test regist"
	  (regist-new-history-record store (make-fn-add-move 1 2))
	  (test-count store 1)
	  (test-record records 0 0 1 2)
	  
	  (regist-new-history-record store (make-fn-add-move 3 4))
	  (test-count store 2)
	  (test-record records 0 0 1 2)
	  (test-record records 1 0 3 4))
	
	(prove:subtest
	    "Test pop and re-regist"
	  (prove:is-type (pop-history-record store) 'history-record)
	  (test-count store 1)

	  (regist-new-history-record store (make-fn-add-move 5 6))
	  (test-count store 2)
	  (test-record records 0 0 1 2)
	  (test-record records 1 0 5 6)

	  (prove:is-type (pop-history-record store) 'history-record)
	  (prove:is-type (pop-history-record store) 'history-record)
	  (test-count store 0)
	  (prove:ok (not (pop-history-record store)))
	  (test-count store 0))))

    (prove:subtest
	"Test do-history-record-store"
      (let ((store (make-history-record-store))
	    (loop-count 0))
	(regist-new-history-record store (make-fn-add-move 1 2))
	(regist-new-history-record store (make-fn-add-move 3 4))
	(do-history-record-store (record store)
	  (prove:is (get-nth-move (history-record-reverse-list record) 0)
		    (if (= loop-count 0)
			(make-a-move 3 4)
			(make-a-move 1 2))
		    :test #'equalp)
	  (incf loop-count))
	(prove:is loop-count 2)))))

(prove:finalize)
