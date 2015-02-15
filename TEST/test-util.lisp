; this record from http://tsplans.com/i/othello/k.cgi?t=0&m=&k=d3c3c4c5f6f5e6e3f4f3e2f1f2g3h4h3h2g4b4b3d1g6g5h6c6a4b6b5d6a7a5a6d2c2c1f7f8c7c8e7d7b2g7h7a1d8a2h8a3g8e8b8a8b788h588h188g288g188b188e1
(defparameter *test-record*
  '((3 . 2) (2 . 2) (2 . 3) (2 . 4) (5 . 5) (5 . 4) (4 . 5) (4 . 2)
    (5 . 3) (5 . 2) (4 . 1) (5 . 0) (5 . 1) (6 . 2) (7 . 3) (7 . 2)
    (7 . 1) (6 . 3) (1 . 3) (1 . 2) (3 . 0) (6 . 5) (6 . 4) (7 . 5)
    (2 . 5) (0 . 3) (1 . 5) (1 . 4) (3 . 5) (0 . 6) (0 . 4) (0 . 5)
    (3 . 1) (2 . 1) (2 . 0) (5 . 6) (5 . 7) (2 . 6) (2 . 7) (4 . 6)
    (3 . 6) (1 . 1) (6 . 6) (7 . 6) (0 . 0) (3 . 7) (0 . 1) (7 . 7)
    (0 . 2) (6 . 7) (4 . 7) (1 . 7) (0 . 7) (1 . 6) (7 . 4) (7 . 0)
    (6 . 1) (6 . 0) (1 . 0) (4 . 0)))

(defun make-nth-test-game (n)
  (let ((game (init-game)))
    (labels ((f (record n)
	       (if (or (<= n 0) (null record))
		   (return-from f game))
	       (move-game game (caar record) (cdar record))
	       (f (cdr record) (- n 1))))
      (f *test-record* n))))

(defmacro within (val1 val2 epsilon)
  `(prove:ok (<= (abs (- ,val1 ,val2)) ,epsilon)))

(defmacro prove-in (val list)
  `(prove:ok (member ,val ,list)))
