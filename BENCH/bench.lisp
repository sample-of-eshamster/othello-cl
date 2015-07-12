(load "BENCH/bench-utils.lisp")
(load "TEST/test-util.lisp")

(defmacro do-in-some-boards (&body body)
  (let ((i (gensym)))
  `(dolist (,i '(0 10 21 30 41 50 55))
     (let ((game (make-nth-test-game ,i)))
       ,@body))))

(defun bench-a-player (title plyr)
  (do-bench title
    (do-in-some-boards
      (funcall (player-make-mover plyr) game))))

(defun bench-mc ()
  (let ((plyr (construct-player "test" 'mc)))
    (player-set-param plyr 'times 3000)
    (player-set-param plyr 'fn-make-policy #'make-uniform-policy)
    (bench-a-player "mc-3000" plyr)))

(defun bench-uct ()
  (let ((plyr (construct-player "test" 'uct)))
    (player-set-param plyr 'times 3000)
    (player-set-param plyr 'ucb-coef (sqrt 2))
    (player-set-param plyr 'expand-intv 2)
    (player-set-param plyr 'fn-make-policy #'make-uniform-policy)
    (bench-a-player "uct-3000" plyr)))

(defun bench-minimax ()
  (let ((plyr (construct-player "test" 'minimax)))
    (player-set-param plyr 'depth 6)
    (bench-a-player "minimax-6" plyr)))

(defparameter *bench-list* (list #'bench-mc
				 #'bench-uct
				 #'bench-minimax))

(defun do-all-bench (title)
  (let ((new-bench-all (make-bench-all)))
    (setf (bench-all-title new-bench-all) title)
    (setf (bench-all-list new-bench-all)
	  (mapcar #'(lambda (fn) (funcall fn)) *bench-list*))
    (append (restore-bench-all-lst *bench-dump-file*) (list new-bench-all))))
