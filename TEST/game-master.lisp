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
      "Test add-player"
    (let* ((plyr-list (t-make-player-list))
	   (first-len (length plyr-list)))
      (labels ((test (test-title plyr format-str expected-len expected-match)
		 (prove:subtest
		     test-title
		   (let ((name (player-name plyr))
			 (new-lst (com-add-player plyr plyr-list
						  (make-string-input-stream
						   (format nil format-str)))))
		     (prove:is (length new-lst) expected-len)
		     (prove:ok (not (null (find-player-by-name name new-lst))))
		     (prove:is (equalp (find-player-by-name name new-lst)
				       (find-player-by-name name plyr-list))
			       expected-match))
		   (prove:is (length plyr-list) first-len))))
	(test "Add new player"
	      (construct-player "new-name" 'human)
	      "" (1+ first-len) nil)
	(test "Overwrite an existing player"
	      (construct-player "test2" 'human)
	      "y" first-len nil)
	(test "Don't overwrite an existeng player"
	      (construct-player "test2" 'human)
	      "n" first-len t))))

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
	(test kind))))
  (prove:subtest
      "Test com-player (not comprehensive)"
    (labels ((test-success (com-list expected)
	       (let ((plyr-list (t-make-player-list)))
		 (multiple-value-bind (lst suc)
		     (com-player com-list plyr-list)
		   (prove:is suc expected)))))
      ; add, remove are not tested
      (test-success '(show) t)
      (test-success '(help) t)
      (test-success nil t)
      (test-success '(not-defined) nil))))

(prove:finalize)
