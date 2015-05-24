(defun string-to-list (line)
  (let ((result nil))
    (with-input-from-string (s line)
      (labels ((add-to-list ()
		 (let ((value (read s nil)))
		   (if (null value) (return-from add-to-list))
		   (setf result (cons value result))
		   (add-to-list))))
	(add-to-list)))
    (reverse result)))

(defun stream-to-list (&optional (stream *standard-input*))
  (string-to-list (read-line stream)))

(defmethod to-string ((target string)) target)
(defmethod to-string ((target number)) (format nil "~D" target))
(defmethod to-string ((target function)) (symbol-name (function-name target)))
(defmethod to-string ((target symbol)) (symbol-name target))

(defun concat-symbol (&rest symbols)
  (if (= (length symbols) 0)
      (return-from concat-symbol nil))
  (let ((str ""))
    (dolist (x symbols)
      (setf str (concatenate 'string str (symbol-name x))))
    (intern str)))

(defun push-without-dup (target lst fn-equal)
  (if (null target)
      (return-from push-without-dup))
  (labels ((f (item old-lst new-lst)
	     (if (null (car old-lst))
		 (return-from f 
		   (if (null item)
		       new-lst
		       (cons item new-lst))))
	     (if (or (null item)
		     (not (funcall fn-equal item (car old-lst))))
		 (setf new-lst (cons (car old-lst) new-lst))
		 (progn (setf new-lst (cons item new-lst)) (setf item nil)))
	     (f item (cdr old-lst) new-lst)))
    (reverse (f target lst nil))))

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
