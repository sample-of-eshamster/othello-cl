(ql:quickload 'cl-ppcre)

; This is written for ClozureCL (output of time macro is like followings)
; 
; took 6 microseconds (0.000006 seconds) to run.
; During that period, and with 2 available CPU cores,
;      0 microseconds (0.000000 seconds) were spent in user mode
;      0 microseconds (0.000000 seconds) were spent in system mode
;  32 bytes of memory allocated.

(defstruct bench
  :total-msec
  :user-msec
  :sys-msec
  :mem-byte)

(defmacro extract-bench-result (reg strs &body body)
  (let ((line (gensym)))
    `(dolist (,line ,strs)
       (let ((it nil))
	 (when (setf it (ppcre:scan-to-strings ,reg ,line))
	   ,@body)))))

(defmacro do-bench (&body body)
  (let ((s (gensym))
	(split (gensym))
	(b (gensym)))
    `(labels ((extract-msec (str)
		(when (null str)
		  (return-from extract-msec 0))
		(* 1000
		   (read-from-string
		    (ppcre:scan-to-strings "[0-9]*\\.[0-9]*" str)))))
       (let ((,s nil)
	     (,split nil)
	     (,b (make-bench)))
	 (setf ,s (with-output-to-string (*trace-output*) (time (progn ,@body))))
	 (setf ,split (ppcre:split "\\n" ,s))
	 
	 (extract-bench-result "took.*to run." ,split
	   (setf (bench-total-msec ,b) (extract-msec it)))
	 (extract-bench-result ".*in user mode" ,split
	   (setf (bench-user-msec ,b) (extract-msec it)))
	 (extract-bench-result ".*in system mode" ,split
	   (setf (bench-sys-msec ,b) (extract-msec it)))
	 
	 (extract-bench-result ".*memory allocated" ,split
	   (setf (bench-mem-byte ,b)
		 (parse-integer
		  (ppcre:regex-replace-all
		   "," (ppcre:scan-to-strings "[0-9,]+" it) ""))))
	 ,b))))

