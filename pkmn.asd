;;;; pkmn.asd

(asdf:defsystem #:pkmn
  :description "pokemon battle clone written in common lisp"
  :author      "Akah"
  :license     "gplv3.0"
  :version     "0.0.1"
  :serial      t
  :depends-on (#:cl-opengl
	       #:sdl2
	       #:sdl2-image)
  :components ((:module
		#:res
		:components ((:static-file "res/img/espeon-front.png")
			     (:static-file "res/img/espeon-back.png")
			     (:static-file "res/img/umbreon-front.png")
			     (:static-file "res/img/umbreon-back.png")))
		(:module
		 #:src
		 :components ((:file "package")
			      (:file "src/data/structs")
			      (:file "src/battle/effectiveness")
			      (:file "src/battle/battle")
			      (:file "src/game")
			      (:file "src/pkmn")))))
  

