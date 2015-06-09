; (defpackage
;     (:use :cl :asdf))

; (in-package :othello-asd)

(ql:quickload :cl-ppcre)
(ql:quickload :prove)

(defsystem "othello"
  :description "lisp othello written for learning common lisp"
  :version "0.0.1"
  :author "eshamster <hamgoostar@gmail.com>"
  :components ((:file "define")
	       (:file "utils")
	       (:file "tree")
	       (:file "move")
	       (:file "board")
	       (:file "game")
	       (:file "eval-board")
	       (:file "minimax")
	       (:file "random-move")
	       (:file "mc")
	       (:file "uct")
	       (:file "human")
	       (:file "player")
	       (:file "game-master"))
  :serial t)
