(defvar *canvas-width* 800)
(defvar *canvas-height* 600)
(defvar *title* "pokémon")

(gamekit:defgame game () ()
		 (:viewport-width  *canvas-width*)
		 (:viewport-height *canvas-height*)
		 (:viewport-title  *title*))

(gamekit:register-resource-package :keyword "../res/img/")

(defun start-game ()
  (gamekit:start 'game))

(defun kill-game ()
  (gamekit:stop))
