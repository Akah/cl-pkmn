;;;; pkmn.asd

(asdf:defsystem #:pkmn
  :description "pokemon battle clone written in common lisp"
  :author      "akahlakahdakah"
  :license     "gplv3.0"
  :version     "0.0.1"
  :serial      t
  :components ((:file "package")
	       (:file "structs")
               (:file "pkmn")
	       (:file "battle")
	       (:file "effectiveness")))
