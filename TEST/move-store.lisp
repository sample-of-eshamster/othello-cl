(prove:plan 1)

(prove:subtest
    "Test get-nth-move"
  (let ((store (init-move-store)))
    (add-to-move-store store 1 2)
    (add-to-move-store store 3 4)
    
    (prove:ok (not (get-nth-move store -1)))
    (prove:ok (not (get-nth-move store 10)))
    (prove:ok (not (get-nth-move nil 0)))
    (prove:is (get-nth-move store 1) (make-a-move 3 4))))

(prove:finalize)
