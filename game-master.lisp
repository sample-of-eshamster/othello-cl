(defparameter *player-file* "PLAYER_INFO")

(defun game-loop (game white-mover black-mover)
  (loop until (is-game-end game) do
       (if (= (game-turn game) *white*)
	   (funcall white-mover game)
	   (funcall black-mover game))))  

;-----commands about player -----;

; TODO: change the interface of adding player (to receive a player's name)
(defun com-player (com-list plyr-list &optional (stream *standard-input*))
  (let ((valid-com t)
	(com-name (car com-list)))
    (case com-name
      ((nil help) (com-player-help))
      (show (com-show-player plyr-list))
      (remove (multiple-value-bind (lst suc) (com-remove-player (symbol-name (cadr com-list)) plyr-list)
	      (if suc
		  (progn (setf plyr-list lst))
		  (format t "The name \"~D\" is not exist~%" (cadr com-list)))))
      (add (setf plyr-list (com-add-player (com-init-player) plyr-list stream)))
      (t (format t "The command \"~D\" is not defined" com-name)
	 (setf valid-com nil)))
    (values plyr-list valid-com)))

(defun com-player-help ()
  (labels ((princ-line (str)
	     (princ (concatenate 'string "player " str))
	     (fresh-line)))
    (princ-line "show")
    (princ-line "remove [name]")
    (princ-line "add")))

(defun com-load-player (file-name)
  (with-open-file (in file-name :direction :input)
    (let (str result)
      (loop while (setf str (read-line in nil)) do
	   (setf result (cons (player-deserialize str) result)))
      result)))

(defun com-save-player (file-name player-list)
  (with-open-file (out file-name
		       :direction :output
		       :if-exists :new-version
		       :if-does-not-exist :create)
    (dolist (plyr player-list)
      (princ (player-serialize plyr) out)
      (fresh-line out))))

(defun com-show-player (player-list &optional (stream *standard-output*))
  (dolist (plyr player-list)
    (format stream "~A (~A) -> " (player-name plyr) (player-kind plyr))
    (maphash #'(lambda (k v)
		 (format stream "~A = ~A, " k (to-string v)))
	     (player-params plyr))
    (fresh-line stream)))

(defun com-remove-player (name player-list)
  (when (equalp name "")
    (princ "Please input the name to delete")
    (return-from com-remove-player (values player-list nil)))
  (let ((found (find-player-by-name name player-list)))
    (when (null found)
      (format t "The name '~D' is not found~%" name)
      (return-from com-remove-player (values player-list nil)))
    (values (remove found player-list) t)))

(defun com-add-player (plyr plyr-list &optional (stream *standard-input*))
  (if (null plyr)
      (return-from com-add-player plyr-list))
  (when (find-player-by-name (player-name plyr) plyr-list)
    (unless (equalp
	     (read-line-while "The name is already exist. Do you overwrite it? [y/n]"
			      #'(lambda (str) (not (or (equalp str "y")
						       (equalp str "n"))))
			      stream)
	     "y")
      (return-from com-add-player plyr-list)))
  (com-modify-player plyr stream)
  (push-without-dup plyr plyr-list #'(lambda (a b) (equalp (player-name a) (player-name b)))))

(defun com-init-player (&optional (stream *standard-input*))
  (let* ((name (read-line-while "name"
				#'(lambda (str)
				    (equalp str ""))
				stream))
	 (plyr nil))
    (read-line-while "kind [human, random, mc, uct]"
		     #'(lambda (str)
			 (setf plyr (construct-player name (intern (string-upcase str))))
			 (null plyr))
		     stream)
    plyr))

(defun com-modify-player (plyr &optional (stream *standard-input*))
  (maphash #'(lambda (k v)
	       (read-line-while (format nil "~D (default=~D)" k (to-string v))
				#'(lambda (str)
				    (not (or (equalp str "")
					     (handler-case
						 (player-set-param plyr k
								   (read-from-string str))
					       (simple-error (e) nil)))))
				stream))
	   (player-params plyr))
  plyr)

