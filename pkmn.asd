;;;; pkmn.asd

(asdf:defsystem #:pkmn
  :description "pokemon clone written in common lisp"
  :author      "Akah"
  :license     "gplv3.0"
  :version     "0.0.1"
  :serial      t
  :depends-on (sdl2
	       cl-opengl)
  :components ((:file "package")
	       (:file "src/data/structs")
	       (:file "src/battle/effectiveness")
	       (:file "src/battle/battle")
               (:file "src/pkmn")))
