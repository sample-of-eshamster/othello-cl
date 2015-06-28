; TODO: make tests

(defparameter *max-reverse-list* (* (- *board-size* 2) 3) )
(defparameter *max-history-record* (- (* *board-size* *board-size*) 4))

(defstruct history-record
  (turn *empty*)
  (move (make-a-move -1 -1))
  (reverse-list (init-move-store :num-moves *max-reverse-list*)))

(defstruct history-record-store
  (count 0)
  (records (make-array *max-history-record*
		       :initial-contents (let ((lst nil))
					   (dotimes (i *max-history-record*)
					     (setf lst (cons (make-history-record) lst)))
					   lst))))

(defun regist-new-history-record (store fn-process-record)
  (let ((record (aref (history-record-store-records store)
		      (history-record-store-count store))))
    (reset-move-store (history-record-reverse-list record))
    (when (funcall fn-process-record record)
      (incf (history-record-store-count store)))))

(defun pop-history-record (store)
  (if (<= (history-record-store-count store) 0)
      (return-from pop-history-record nil))
  (decf (history-record-store-count store))
  (aref (history-record-store-records store)
	(history-record-store-count store)))
