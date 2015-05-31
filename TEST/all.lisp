(ql:quickload :prove)

(defparameter *reporter* :dot)

(defparameter *test-list* (list "define"
				"utils"
				"tree"
				"move"
				"board"
				"game"
				"eval-board"
				"minimax"
				"random-move"
				"mc"
				"uct"
				"human"
				"player"
				"game-master"))

(defparameter *result-list* nil)

(dolist (target *test-list*)
  (format t "#### Test ~A ####~%" target)
  (setf *result-list*
	(cons
	 (list target
	       (prove:run
		(pathname (format nil "TEST/~A.lisp" target))
		:reporter *reporter*))
	 *result-list*)))

(setf *result-list* (reverse *result-list*))
(dolist (result *result-list*)
  (format t "~15@A ... ~A~%"
	  (car result)
	  (if (cadr result) "ok" "FAILED")))
