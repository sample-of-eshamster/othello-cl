(prove:plan 1)

(prove:subtest
 "Test lazy evaluation"
 
 (prove:subtest
  "Test if it is evaluated only once"
  (defparameter *x* (lazy (princ "first") (+ 100 200)))
  (prove:is-print (force *x*) "first")
  (prove:is-print (force *x*) ""))
 
 (prove:subtest
  "Test if output is immutable"
  (defparameter *x* (lazy (print 'first) (+ 100 200)))
  (prove:is (force *x*) 300)
  (prove:is (force *x*) 300))

 (prove:subtest
  "Test lazy-car lazy-cdr"
  (defparameter *x* (lazy '(1 2 3)))
  (prove:is (lazy-car *x*) 1)
  (prove:is (lazy-cdr *x*) '(2 3)))

 (prove:subtest
  "Test lazy-setf-cdr"
  (defparameter *x* (lazy '(1 2 3)))
  (prove:is (lazy-setf-cdr *x*) '(2 3))
  (prove:is (force *x*) '(2 3))))

(prove:finalize)
