;; ; --------- human --------- ;

;; (defun read-command ()
;;   (let ((line (read-line))
;; 	(result nil))
;;     (with-input-from-string (s line)
;;       (labels ((add-to-list ()
;; 		 (let ((value (read s nil)))
;; 		   (if (null value) (return-from add-to-list))
;; 		   (setf result (cons value result))
;; 		   (add-to-list))))
;; 	(add-to-list)))
;;     (reverse result)))

;; (defun eval-play-command (game com-list)
;;   (case (car com-list)
;;     (print (print-game game) nil)
;;     (reverse (reverse-game game))
;;     (move (if (eq 2 (length (cdr com-list)))
;; 	       (move-game game (cadr com-list) (caddr com-list))
;; 	       nil))
;;     (t (princ "This command is not defined.")
;;        nil)))

;; (defun move-by-human(game)
;;   (print-game game nil)
;;   (if (is-game-end game)
;;       (progn (print "This game has ended")
;; 	     (return-from move-by-human))
;;   (loop while (not (eval-play-command game (read-command))))))

;; ; --------- minimax --------- ;
;; (defun move-by-minimax(game depth)
;;   (if (is-game-end game)
;;       (return-from move-by-minimax))
;;   (let ((move (select-move-by-minimax game depth #'eval-game-by-minimax)))
;;     (move-game game (car move) (cdr move))))

;; ; --------- random --------- ;

;; (defun move-by-uniform-random(game)
;;   (if (is-game-end game)
;;       (return-from move-by-uniform-random))
;;   (move-by-random-policy game #'make-uniform-policy))

;; (defun move-by-uniform-mc(game times)
;;   (if (is-game-end game)
;;       (return-from move-by-uniform-mc))
;;   (let ((move (mc-simulate game #'make-uniform-policy times)))
;;     (move-game game (car move) (cdr move))))
