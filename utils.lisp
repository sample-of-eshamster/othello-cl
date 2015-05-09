(defun stream-to-list (&optional (stream *standard-input*))
  (let ((line (read-line stream))
	(result nil))
    (with-input-from-string (s line)
      (labels ((add-to-list ()
		 (let ((value (read s nil)))
		   (if (null value) (return-from add-to-list))
		   (setf result (cons value result))
		   (add-to-list))))
	(add-to-list)))
    (reverse result)))

; ---- Lazy library ---- ;

(defmacro lazy (&body body)
  (let ((value (gensym))
	(evaluated (gensym)))
    `(let ((,value nil)
	   (,evaluated nil))
       (lambda ()
	 (unless ,evaluated
	   (setf ,value (progn ,@body))
	   (setf ,evaluated t))
	 ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-car (l-lst)
  `(car (force ,l-lst)))

(defmacro lazy-cdr (l-lst)
  `(cdr (force ,l-lst)))

; If l-lst is lazy func, this macro should avoid double evaluation
(defmacro lazy-setf-cdr (l-lst)
  (let ((temp (gensym)))
    `(let ((,temp (lazy-cdr ,l-lst)))
       (setf ,l-lst (lazy ,temp))
       ,temp)))

(defun lazy-null (l-lst)
  (null (force l-lst)))
