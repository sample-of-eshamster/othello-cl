(ql:quickload :prove)

(defparameter *reporter* :tap)

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

(defun check-prove-result (str)
  (let ((result-line (car (last (ppcre:split "\\n" str)))))
    (if (ppcre:all-matches "passed" result-line)
	t
	(progn (format t "~A~%" str)
	       nil))))

(defparameter *result-list* nil)
(defparameter *str-out* (make-string-output-stream))
(setf prove:*test-result-output* *str-out*)

(dolist (target *test-list*)
  (format t "#### Test ~A ####~%" target)
  (prove:run
   (pathname (format nil "TEST/~A.lisp" target))
   :reporter *reporter*)
  (setf *result-list*
	(cons
	 (list target
	       (check-prove-result (get-output-stream-string *str-out*)))
	 *result-list*)))

(setf prove:*test-result-output* t)

(setf *result-list* (reverse *result-list*))
(fresh-line)
(dolist (result *result-list*)
  (format t "~15@A ... ~A~%"
	  (car result)
	  (if (cadr result) "ok" "FAILED")))
