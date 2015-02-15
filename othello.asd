; (defpackage
;     (:use :cl :asdf))

; (in-package :othello-asd)

(defsystem "othello"
  :description "lisp othello written for learning common lisp"
  :version "0.0.1"
  :author "eshamster <hamgoostar@gmail.com>"
  :components ((:file "define")
	       (:file "tree")
	       (:file "move")
	       (:file "board")
	       (:file "game"))
  :serial t)
