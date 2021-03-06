
(defmacro get-node-value (node)
  `(car ,node))

(defmacro get-children (node)
  `(cdr ,node))

(defmacro get-rest-children (children)
  `(cdr ,children))

(defmacro get-nth-child (n node)
  `(nth ,n (get-children ,node)))

(defmacro do-children (name-tree &body body)
  `(dolist (,(car name-tree) (get-children ,(cadr name-tree))) ,@body))

(defun has-children (node)
  (not (null (get-children node))))

(defun add-or-insert-child (parent destructive children)
  (labels ((add-or-insert (lst1 lst2)
	     (if destructive (nconc lst1 lst2) (append lst1 lst2))))
    (add-or-insert
     parent
     (mapcar (lambda (child)
	       (if (and (not (null child)) (listp child) (listp (cdr child)))
		   `,child
		   `(,child))) children))))
 
; not destructive 
(defun add-child (parent &rest children)
  (add-or-insert-child parent nil children))

; destructive
(defun insert-child (parent &rest children)
  (add-or-insert-child parent t children))

(defun get-num-children (node)
  (- (length node) 1))

(defun get-tree-size (tree)
  (labels ((f (node)
 	     (if (null node) (return-from f 0))
 	     (let ((sum 1))
	       (do-children (child node)
		 (incf sum (f child)))
	       sum)))
  (f tree)))

(defun get-tree-depth (tree)
  (labels ((f (node)
	     (if (null node) (return-from f 0))
	     (let ((depth 0))
	       (do-children (child node)
		 (setf depth (max depth (+ (f child) 1))))
	       depth)))
    (f tree)))

(defun print-tree (tree &key (max-depth -1) (f-proc-value #'(lambda (v) v)))
  (labels ((print-str-seq (s n)
	     (dotimes (i n) (princ s)))
	   (f (node depth)
	     (if (or (null node)
		     (and (>= max-depth 0) (> depth max-depth)))
		 (return-from f))
	     (print-str-seq "| " depth)
	     (princ (funcall f-proc-value (get-node-value node)))
	     (fresh-line)
	     (do-children (child node)
	       (f child (+ depth 1)))))
    (f tree 0)))
	     

(defun select-max-child (fn-calc-value parent)
  (if (not (has-children parent))
      (return-from select-max-child nil))
  (select-max-node #'(lambda (node)
		       (funcall fn-calc-value (get-node-value node)))
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
  
