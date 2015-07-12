(defclass player ()
  ((name :accessor player-name :initarg :name)
   (kind :accessor player-kind :initarg :kind)
   (params :accessor player-params :initform (make-hash-table))))

(defmacro setf-player-params-for-init (plyr &rest params)
  `(progn ,@(mapcar #'(lambda (kv)
			`(setf (gethash ',(car kv) (player-params ,plyr)) ,(cadr kv)))
		    params)
	  ,plyr))
(defmacro extract-player-params (plyr &rest extracts)
  `(progn ,@(mapcar #'(lambda (key-target)
			`(setf ,(car key-target)
			       (gethash ',(cadr key-target) (player-params ,plyr))))
		    extracts)))

(defmethod init-player-params ((plyr player)) plyr)

(defun construct-player (name kind)
  (handler-case 
      (let ((result (make-instance (concat-symbol kind '-player))))
	(setf (player-name result) (to-string name))
	(setf (player-kind result) kind)
	(init-player-params result)
	result)
    (simple-error (e) nil)))
  
(defmethod player-serialize ((target player))
  (let ((result nil))
    (setf result (concatenate 'string (player-name target) " " (to-string (player-kind target)) " "))
    (maphash #'(lambda (k v)
		   (setf result (concatenate 'string result (symbol-name k) " " (to-string v) " ")))
	     (player-params target))
    result))

(defun player-deserialize (str)
  (let* ((lst (string-to-list str))
	 (name (car lst))
	 (kind (cadr lst))
	 (params (cddr lst))
	 (result (construct-player name kind)))
    (labels ((set-params (lst)
	       (if (null lst) (return-from set-params result))
	       (let ((k (car lst))
		     (v (cadr lst)))
		 (player-set-param result k v)) params
		 (set-params (cddr lst))))
      (set-params params))))

(defmethod fit-type-to ((right-value number) (target number))
  target)
(defmethod fit-type-to ((right-value number) target)
  (error (format nil "The value (~D) is must be number" target)))
(defmethod fit-type-to ((right-value string) (target symbol))
  (symbol-name target))
(defmethod fit-type-to ((right-value function) (target symbol))
  (coerce target 'function))
(defmethod fit-type-to ((right-value function) (target function))
  target)
(defmethod fit-type-to (right-value (target symbol))
  target)

(defmethod player-set-param ((target player) (key symbol) value)
  (multiple-value-bind (old-value exists) (gethash key (player-params target))
    (unless exists
      (error (format t "The key (~A) is not exists" key)))
    (setf (gethash key (player-params target)) (fit-type-to old-value value))))

(defun find-player-by-name (name lst)
  (find-if #'(lambda (plyr) (equalp name (player-name plyr))) lst))

; --------- human --------- ;

(defclass human-player (player) ())

(defmethod player-make-mover ((target human-player))
  #'(lambda (game &optional (stream *standard-input*)) (move-by-human game stream)))

(defun move-by-human(game &optional (stream *standard-input*))
  (print-game game nil)
  (if (is-game-end game)
      (progn (print "This game has ended")
	     (return-from move-by-human))
      (let ((result))
	(loop while (not (setf result (eval-play-command game (stream-to-list stream)))))
	result)))

; --------- minimax --------- ;

(defclass minimax-player (player) ())

(defmethod init-player-params ((plyr minimax-player))
  (setf-player-params-for-init plyr
			       (depth 4)))

(defmethod player-make-mover ((target minimax-player))
  #'(lambda (game)
      (move-by-minimax game (gethash 'depth (player-params target)))))

(defun move-by-minimax(game depth)
  (if (is-game-end game)
      (return-from move-by-minimax))
  (let ((move (select-move-by-minimax game depth #'eval-game-by-ab)))
    (move-game game (car move) (cdr move))))

; --------- random --------- ;

(defclass random-player (player) ())

(defmethod init-player-params ((plyr random-player)))

(defmethod player-make-mover ((target random-player))
  #'(lambda (game)
      (move-by-uniform-random game)))

(defun move-by-uniform-random(game)
  (if (is-game-end game)
      (return-from move-by-uniform-random))
  (move-by-random-policy game #'make-uniform-policy))

; --------- mc --------- ;

(defclass mc-player (player) ())

(defmethod init-player-params ((plyr mc-player))
  (setf-player-params-for-init plyr
			       (times 100)
			       (fn-make-policy #'make-uniform-policy)))

(defmethod player-make-mover ((plyr mc-player))
  (let (ts fn)
    (extract-player-params plyr
			   (ts times)
			   (fn fn-make-policy))
    #'(lambda (game)
	(move-by-mc game fn ts))))

(defun move-by-mc(game fn-make-policy times)
  (if (is-game-end game)
      (return-from move-by-mc))
  (let ((move (mc-simulate game fn-make-policy times)))
    (move-game game (car move) (cdr move))))

; --------- uct --------- ;

(defclass uct-player (player) ())

(defmethod init-player-params ((plyr uct-player))
  (let ((par (make-def-uct-param)))
    (setf-player-params-for-init plyr
				 (expand-intv (uct-param-expand-intv par))
				 (ucb-coef (uct-param-ucb-coef par))
				 (fn-make-policy (uct-param-fn-make-policy par))
				 (times 100))))

(defmethod player-make-mover ((plyr uct-player))
  (let ((par (make-uct-param)) ts)
    (extract-player-params plyr
			   (ts times)
			   ((uct-param-expand-intv par) expand-intv)
			   ((uct-param-ucb-coef par) ucb-coef)
			   ((uct-param-fn-make-policy par) fn-make-policy))
    #'(lambda (game)
	(move-by-uct game par ts))))

(defun move-by-uct(game uct-par times)
  (if (is-game-end game)
      (return-from move-by-uct))
  (let ((move (uct-simulate game uct-par times)))
    (move-game game (car move) (cdr move))))
