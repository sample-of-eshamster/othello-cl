(defparameter *max-move-store* (- (* *board-size* *board-size*) 4))

(defstruct move-store
  :count
  :moves)

(defun init-move-store (&key (num-moves *max-move-store*))
  (make-move-store :count 0
		   :moves (make-array num-moves
				      :initial-contents (let ((lst nil))
							  (dotimes (i num-moves)
							    (setf lst (cons (make-a-move 0 0) lst)))
							  lst))))

(defun reset-move-store (store)
  (setf (move-store-count store) 0)
  store)

; skip the range check
(defun add-to-move-store (store x y)
  (set-to-move (aref (move-store-moves store) (move-store-count store)) x y)
  (incf (move-store-count store))
  store)

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
    (add-to-move-store dst (car move) (cdr move)))
  dst)
(defun clone-move-store (src)
  (let ((dst (init-move-store)))
    (copy-move-store dst src)
    dst))

(defstruct move-store-stack
  :unused-store
  :used-store)

(defun num-allocated-move-store-stack (stack)
  (+ (length (move-store-stack-used-store stack))
     (length (move-store-stack-unused-store stack))))

(defun num-reserved-move-store-stack (stack)
  (length (move-store-stack-used-store stack)))

(defun reserve-move-store-from-stack (stack)
  (macrolet ((unused (stack) `(move-store-stack-unused-store ,stack))
	     (used (stack) `(move-store-stack-used-store ,stack)))
    (when (null (unused stack))
      (setf (used stack) (cons (init-move-store) (used stack)))
      (return-from reserve-move-store-from-stack (car (used stack))))
    (setf (used stack) (cons (car (unused stack)) (used stack)))
    (setf (unused stack) (cdr (unused stack)))
    (car (used stack))))

(defun free-move-store-to-stack (stack)
  (macrolet ((unused (stack) `(move-store-stack-unused-store ,stack))
	     (used (stack) `(move-store-stack-used-store ,stack)))
    (setf (unused stack) (cons (car (used stack)) (unused stack)))
    (setf (used stack) (cdr (used stack)))))

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

(defun mapcar-move-store (fn store)
  (let ((lst nil))
    (do-move-store (move store)
      (setf lst (cons (funcall fn move) lst)))
  lst))
