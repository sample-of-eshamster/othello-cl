(defun game-loop (game white-mover black-mover)
  (loop until (is-game-end game) do
       (if (= (game-turn game) *white*)
	   (funcall white-mover game)
	   (funcall black-mover game))))
  
