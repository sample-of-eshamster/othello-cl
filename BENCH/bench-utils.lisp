
; 
; took 6 microseconds (0.000006 seconds) to run.
; During that period, and with 2 available CPU cores,
;      0 microseconds (0.000000 seconds) were spent in user mode
;      0 microseconds (0.000000 seconds) were spent in system mode
;  32 bytes of memory allocated.

(defparameter *temp-bench-result-file* "BENCH/temp_bench_result")
(defparameter *bench-result-file* "BENCH/bench_result")
(defparameter *bench-dump-file* "BENCH/bench_dump") ; for (eval)

(defstruct bench
  (title "")
  (total-msec 0)
  (user-msec 0)
  (sys-msec 0)
  (mem-byte 0))

(defstruct bench-all
  (title "")
  (list nil))

(defun add-bench-result (b1 b2)
  (let ((b (make-bench)))
    (macrolet ((add-value (getter)
		 `(setf (,getter b) (+ (,getter b1) (,getter b2)))))
      (add-value bench-total-msec)
      (add-value bench-user-msec)
      (add-value bench-sys-msec)
      (add-value bench-mem-byte)
      b)))

(defun save-and-dump-bench-all-lst (bench-all-lst)
  (dump-bench-all-lst *bench-dump-file* bench-all-lst)
  (with-open-file (out *bench-result-file*
		       :direction :output
		       :if-exists :new-version
		       :if-does-not-exist :create)
    (dolist (bench-all bench-all-lst)
      (print-bench-all bench-all :out out))))
  

(defun dump-bench-all-lst (file-name bench-all-lst)
  (with-open-file (out file-name
		       :direction :output
		       :if-exists :new-version
		       :if-does-not-exist :create)
    (print bench-all-lst out)))

(defun restore-bench-all-lst (file-name)
  (with-open-file (in file-name :direction :input)
    (let ((lst (read in nil)))
      (if (and (listp lst) (bench-all-p (car lst)))
	  lst nil))))

(defun print-bench-all (bench-all &key (out *standard-output*))
  (labels ((print-horizontal-line ()
	     (format out "~75,,,'-a~%" '||))
	   (print-bench (res)
	     (format out "~10@a~10:d~10:d~10:d~15:d~%"
		     (bench-title res)
		     (round (bench-total-msec res))
		     (round (bench-user-msec res))
		     (round (bench-sys-msec res))
		     (bench-mem-byte res))))
    (format out "<<~A>>~%" (bench-all-title bench-all))
    (let ((total-res (make-bench))
	  (bench-list (bench-all-list bench-all)))
      (format out "~10@a~10@a~10@a~10@a~15@a~%" 'title 'total[ms] 'user[ms] 'sys[ms] 'mem[byte])
      (print-horizontal-line)
      (dolist (res bench-list)
	(print-bench res)
	(setf total-res (add-bench-result total-res res)))
      (when (> (length bench-list) 1)
	(print-horizontal-line)
	(print-bench total-res)))))
  
(defmacro extract-bench-result (reg strs &body body)
  (let ((line (gensym)))
    `(dolist (,line ,strs)
       (let ((it nil))
	 (when (setf it (ppcre:scan-to-strings ,reg ,line))
	   ,@body)))))

(defmacro do-bench (title &body body)
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
	 (setf (bench-title ,b) ,title)
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

