(prove:plan 2)

(load "TEST/test-util.lisp")

(prove:subtest
    "Test game-loop"
  (labels ((test (game)
	     (game-loop game
			(player-make-mover (construct-player "test-white" 'random))
			(player-make-mover (construct-player "test-black" 'random)))
	     (prove:ok (is-game-end game))))
    (test (init-game))
    (test (make-nth-test-game 15))))

(defun t-make-player-list ()
  (list (construct-player "test1" 'mc)
	(construct-player "test2" 'random)
	(construct-player "test3" 'uct)))

(prove:subtest
    "Test commands about player"
  (prove:subtest
      "Test remove"
    (let ((lst (t-make-player-list)))
      (multiple-value-bind (result test) (com-remove-player "not-found" lst)
	(prove:ok (not test))
	(prove:is (length result) (length lst)))
      (multiple-value-bind (result test) (com-remove-player "" lst)
	(prove:ok (not test))
	(prove:is (length result) (length lst)))
      (multiple-value-bind (result test) (com-remove-player "test2" lst)
	(prove:ok test)
	(prove:is (length result) (1- (length lst)))
	(prove:ok (not (find-if #'(lambda (plyr)
				    (equalp (player-name plyr) "test2"))
				result))))))
      
  (prove:subtest
      "Test save and load"
    (let ((save-list (t-make-player-list))
	  (load-list nil)
	  (file-name "TEST/temp-players"))
      (if (probe-file file-name)
	  (delete-file file-name))
      (assert (not (probe-file file-name)))

      (labels ((get-result ()
		 (com-save-player file-name save-list)
		 (setf load-list (com-load-player file-name))
		 
		 (dolist (plyr save-list)
		   (let ((found (find-if #'(lambda (target)
					     (equalp (player-name plyr)
						     (player-name target)))
					 load-list)))
		     (when (or (null found)
			       (not (eq (type-of plyr) (type-of found))))
			 (return-from get-result nil))))
		 t))
	(prove:ok (get-result))
	; Test overwriting
	(setf save-list (com-remove-player "test2" save-list))
	(assert (= (length save-list) 2))
	(prove:ok (get-result)))))

  (prove:subtest
      "Test show"
    (let ((lst (t-make-player-list))
	  (out1 (make-string-output-stream))
	  (out2 (make-string-output-stream)))
      (com-show-player lst out1)
      (setf lst (cons (construct-player "test4" 'human) lst))
      (com-show-player lst out2)
      (prove:ok (< (length (get-output-stream-string out1))
		   (length (get-output-stream-string out2))))))

  (prove:subtest
      "Test init-player"
    (let ((plyr (com-init-player
		 (make-string-input-stream
		  (format nil "~%  ~%  test  ~%~%  ~%non-player~%  mc ")))))
      (prove:is (player-name plyr) "test")
      (prove:is (type-of plyr) 'mc-player)))

  (prove:subtest
      "Test modify-player (test only default value)"
    (labels ((test (kind)
	       (let ((plyr (construct-player "test" kind))
		     (str ""))
		 (maphash #'(lambda (k v)
			      (setf str (concatenate 'string "~%" str)))
			  (player-params plyr))
		 (prove:ok
		  (com-modify-player plyr (make-string-input-stream
					   (format nil str)))))))
      (dolist (kind '(human random mc uct))
	(test kind)))))

(prove:finalize)
