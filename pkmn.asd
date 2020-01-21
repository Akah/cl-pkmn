;;;; pkmn.asd

(asdf:defsystem #:pkmn
  :description "pokemon battle clone written in common lisp"
  :author      "Akah"
  :license     "gplv3.0"
  :version     "0.0.1"
  :serial      t
  :depends-on (#:cl-opengl
	       #:sdl2
	       #:sdl2-image
	       #:sdl2-mixer)
  :components ((:file "package")
	       (:file "src/data/structs")
	       (:file "src/battle/effectiveness")
	       (:file "src/battle/battle")
	       (:file "src/pkmn")))
