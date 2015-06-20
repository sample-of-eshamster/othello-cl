; TODO: make test
(defparameter *max-move-store* (- (* *board-size* *board-size*) 4))

(defstruct move-store
  :count
  :moves)

(defun init-move-store ()
  (make-move-store :count 0
		   :moves (make-array *max-move-store*
				      :initial-contents (let ((lst nil))
							  (dotimes (i *max-move-store*)
							    (setf lst (cons (make-a-move 0 0) lst)))
							  lst))))

(defun reset-move-store (store)
  (setf (move-store-count store) 0))

(defun add-to-move-store (store x y)
  (set-to-move (aref (move-store-moves store) (move-store-count store)) x y)
  (incf (move-store-count store)))

(defmacro do-move-store (arg &body body)
  (let ((i (gensym))
	(store (gensym)))
  `(let ((,store ,(cadr arg)))
     (dotimes (,i (move-store-count ,store))
       (let ((,(car arg) (aref (move-store-moves ,store) ,i)))
	 ,@body)))))

(defun contains-move (store x y)
  (do-move-store (move store)
    (when (and (eq x (move-x move))
	       (eq y (move-y move)))
      (return-from contains-move t)))
  nil)

(defun copy-move-store (dst src)
  (reset-move-store dst)
  (do-move-store (move src)
    (add-to-move-store dst (car move) (cdr move))))
(defun clone-move-store (src)
  (let ((dst (init-move-store)))
    (copy-move-store dst src)
    dst))

(defstruct move-store-stack
  :unused-store
  :used-store)

(defun num-reserved-move-store-stack (buf)
  (length (move-store-stack-used-store buf)))

(defun reserve-move-store-from-stack (buf)
  (macrolet ((unused (buf) `(move-store-stack-unused-store ,buf))
	     (used (buf) `(move-store-stack-used-store ,buf)))
    (when (null (unused buf))
      (setf (used buf) (cons (init-move-store) (used buf)))
      (return-from reserve-move-store-from-stack (car (used buf))))
    (setf (used buf) (cons (car (unused buf)) (used buf)))
    (setf (unused buf) (cdr (unused buf)))
    (car (used buf))))

(defun free-move-store-to-stack (buf)
  (macrolet ((unused (buf) `(move-store-stack-unused-store ,buf))
	     (used (buf) `(move-store-stack-used-store ,buf)))
    (setf (unused buf) (cons (car (used buf)) (unused buf)))
    (setf (used buf) (cdr (used buf)))))

(defmacro with-cloned-move-store (stack cloned<>store &body body)
  `(let ((,(car cloned<>store) (reserve-move-store-from-stack ,stack)))
     (copy-move-store ,(car cloned<>store) ,(cadr cloned<>store))
     ,@body
     (free-move-store-to-stack ,stack)))

; TODO: change the order of arguments (to equalize with 'nth')
; TODO: change from func to the macro
(defun get-nth-move (store n)
  (cond ((null store) nil)
	((< n 0) nil)
	((>= n (move-store-count store)) nil)
	(t (aref (move-store-moves store) n))))
