(prove:plan 5)

(load "TEST/test-util.lisp")

(prove:subtest
    "Test fit-type-to"
  (prove:subtest
      "number"
    (prove:is (fit-type-to 12 '111) 111)
    (prove:is-error (fit-type-to 12 'not-number) 'simple-error))
  (prove:subtest
      "string"
    (prove:is (fit-type-to "str" 'test) "TEST"))
  (prove:subtest
      "function"
    (prove:is (fit-type-to #'+ '-) #'-)
    (prove:is (fit-type-to #'+ #'-) #'-)
    (prove:is-error (fit-type-to #'+ 'not-defined-func-name) 'undefined-function))
  (prove:subtest
      "others"
    (prove:is (fit-type-to 'abc 'test) 'test)
    (prove:is-error (fit-type-to 'abc "test") 'no-applicable-method-exists)))

(defparameter *all-player-kind* '(human minimax random mc uct))

(prove:subtest
    "Test consturct-player"
  (labels ((test-exist (kind)
	     (prove:ok (subtypep (type-of (construct-player "test" kind)) 'player))))
    (prove:subtest
	"Test for existing player"
      (dolist (kind *all-player-kind*)
	(test-exist kind)))
    (prove:subtest
	"Test for not-existing player"
      (prove:is (construct-player "test" 'not-exist) nil))))

(prove:subtest
    "Test serialize and desirialize player"
  (labels ((test (kind)
	     (let* ((name "test-name")
		    (first (construct-player name kind))
		    (first-dump (player-serialize first))
		    (second (player-deserialize first-dump)))
	       (prove:subtest
		   (format nil "Test ~D" kind)
		 (prove:ok (stringp first-dump))
		 (prove:is first-dump (player-serialize second) :test #'equalp)
		 (maphash #'(lambda (k v)
			      (prove:is (gethash k (player-params second)) v :test #'equalp))
			  (player-params first))))))
    (dolist (kind *all-player-kind*)
      (test kind))))

(prove:subtest
    "Test player-make-mover"
  (let ((start-depth 5))
    (labels ((test (kind &optional (opt nil))
	       (let* ((name "test-name")
		      (plyr (construct-player name kind))
		      (mover (player-make-mover plyr))
		      (game (make-nth-test-game start-depth)))
		 (if (null opt)
		     (prove:ok (funcall mover game))
		     (prove:ok (funcall mover game opt)))
		 (prove:is (get-game-depth game) (1+ start-depth)))))
      (let ((move (get-nth-move (make-moves (make-nth-test-game start-depth)) 0)))
	(test 'human (make-string-input-stream
		      (format nil "print~%move ~D ~D" (car move) (cdr move)))))
      (dolist (kind (remove 'human *all-player-kind*))
	(test kind)))))

(prove:subtest
    "Test find-player-by-name"
  (let* ((found (construct-player "test" 'mc))
	 (lst (list (construct-player "abcd" 'human)
		    found
		    (construct-player "xyz" 'uct))))
    (prove:is (find-player-by-name "test" lst) found :test #'equalp)
    (prove:isnt (find-player-by-name "abcd" lst) found :test #'equalp)
    (prove:ok (null (find-player-by-name "not-found" lst)))))

(prove:finalize)
