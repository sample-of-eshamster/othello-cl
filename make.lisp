(require :asdf)

; compile
(setf asdf:*central-registry*
      '(*default-pathname-defaults*))
(asdf:load-system 'othello)
