(defun game-loop (game white-mover black-mover)
  (loop until (is-game-end game) do
       (if (= (game-turn game) *white*)
	   (funcall white-mover game)
	   (funcall black-mover game))))  

;-----commands about player -----;

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
  (let ((found (find-if #'(lambda (plyr) (equalp (player-name plyr) name)) player-list)))
    (when (null found)
      (format t "The name '~D' is not found~%" name)
      (return-from com-remove-player (values player-list nil)))
    (values (remove found player-list) t)))

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

