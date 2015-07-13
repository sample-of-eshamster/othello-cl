(prove:plan 8)

(prove:isnt *white* *black*)

(prove:subtest
    "Test is-empty"
  (prove:ok (is-empty nil))
  (prove:ok (is-empty 0))
  (prove:ok (not (is-empty *white*)))
  (prove:ok (not (is-empty *black*))))

(prove:subtest
    "Test is-reverse"
  (prove:ok (is-reverse *white* *black*))
  (prove:ok (is-reverse *black* *white*))
  (prove:ok (not (is-reverse *white* *white*)))
  (prove:ok (not (is-reverse *black* *black*)))
  (prove:ok (not (is-reverse *black* nil)))
  (prove:ok (not (is-reverse nil *black*)))
  (prove:ok (not (is-reverse nil nil))))

(prove:subtest
    "Test reverse-turn"
  (prove:is (reverse-turn *white*) *black*)
  (prove:is (reverse-turn *black*) *white*))

(prove:subtest
    "Test is-up-dir"
  (prove:ok (is-up-dir *dir-up*))
  (prove:ok (is-up-dir *dir-right-up*))
  (prove:ok (is-up-dir *dir-left-up*))
  (prove:ok (not (is-up-dir *dir-left-down*))))

(prove:subtest
    "Test is-down-dir"
  (prove:ok (is-down-dir *dir-down*))
  (prove:ok (is-down-dir *dir-right-down*))
  (prove:ok (is-down-dir *dir-left-down*))
  (prove:ok (not (is-down-dir *dir-right-up*))))

(prove:subtest
    "Test is-right-dir"
  (prove:ok (is-right-dir *dir-right*))
  (prove:ok (is-right-dir *dir-right-down*))
  (prove:ok (is-right-dir *dir-right-up*))
  (prove:ok (not (is-right-dir *dir-left-up*))))

(prove:subtest
    "Test is-left-dir"
  (prove:ok (is-left-dir *dir-left*))
  (prove:ok (is-left-dir *dir-left-down*))
  (prove:ok (is-left-dir *dir-left-up*))
  (prove:ok (not (is-left-dir *dir-right-down*))))

(prove:finalize)
