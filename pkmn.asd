;;;; pkmn.asd

(asdf:defsystem #:pkmn
  :description "pokemon clone written in common lisp"
  :author      "Akah"
  :license     "gplv3.0"
  ;; TODO increment version number on each build
  :version     "0.0.1.0"
  :serial      t
  :build-operation "asdf:program-op"
  :entry-point "pkmn:main"
  :depends-on (#:sdl2
	       #:sdl2-image)
  :components ((:file "package")
	       (:file "src/data/structs")
	       (:file "src/battle/effectiveness")
	       (:file "src/battle/battle")
	       (:file "src/graphics/draw-functions")
	       (:file "src/graphics/game")
	       (:file "src/graphics/font")
	       (:file "src/pkmn")))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression 9))
