;;;; pkmn.asd

(asdf:defsystem #:pkmn
  :description "pokemon battle clone written in common lisp"
  :author      "Akah"
  :license     "gplv3.0"
  :version     "0.0.1"
  :serial      t
  :components ((:file "package")
	       (:file "src/data/structs")
               (:file "src/pkmn")
	       (:file "src/battle/battle")
	       (:file "src/battle/effectiveness")))
