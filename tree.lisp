(defun get-node-value (node)
  (car node))

(defun get-children (node)
  (cdr node))

(defun has-children (node)
  (not (null (get-children node))))

(defun add-child (parent child)
  (if (and (not (null child)) (listp child) (listp (cdr child)))
      (append parent `(,child))
      (append parent `((,child)))))

(defun get-num-children (node)
  (- (length node) 1))

(defun select-max-child (fn-calc-value parent)
  (if (not (has-children parent))
      (return-from select-max-child nil))
  (select-max-node #'(lambda (node)
		       (funcall fn-calc-value (car node)))
		   (get-children parent)))

(defun select-max-node (fn-calc-value nodes)
  (let* ((max-node (car nodes))
	 (max-value (funcall fn-calc-value max-node)))
    (dolist (node (cdr nodes))
      (let ((value (funcall fn-calc-value node)))
	(if (< max-value value)
	    (progn (setf max-node node)
		   (setf max-value value)))))
    max-node))
  
