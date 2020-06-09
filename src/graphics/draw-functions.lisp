(in-package #:pkmn)

(defun render-clear (renderer)
  "Reset the screen to white"
  (sdl2:set-render-draw-color renderer 255 255 255 0)
  (sdl2:render-clear renderer))

(defun draw (renderer)
  "Debug only; draw red cube to represent player"
  (sdl2:set-render-draw-color renderer 255 0 0 0)
  (sdl2:render-fill-rect
   renderer (sdl2:make-rect *x* *y* *tile-size* *tile-size*)))

;; TODO ? Move into battle graphics file...
(defun draw-player-stat-holder (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 0)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 300 300 (* *scale* 50) *scale*))
  (sdl2:render-fill-rect renderer (sdl2:make-rect 306 297 9 3))
  (sdl2:render-fill-rect renderer (sdl2:make-rect 312 294 9 6))
  (sdl2:render-fill-rect renderer (sdl2:make-rect 318 291 9 9))
  (sdl2:render-fill-rect renderer (sdl2:make-rect 438 291 15 9))
  (sdl2:set-render-draw-color renderer 70 100 225 0)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 388 291 50 3)))

(defun draw-img (img renderer &key x y w h start-x start-y end-x end-y)
  "draw image from xy to wh with optional start and end for cropping"
  (let* ((texture (sdl2:create-texture-from-surface renderer img))
	 (src-rect (sdl2:make-rect start-x start-y end-x end-y))
	 (dst-rect (sdl2:make-rect (scale x) (scale y) (scale w) (scale h))))
    (sdl2:render-copy renderer texture :source-rect src-rect :dest-rect dst-rect)
    (sdl2:destroy-texture texture)
    (sdl2:free-rect src-rect)
    (sdl2:free-rect dst-rect)))

(defun draw-char (indexes renderer font-image x y)
  "Take in a list of numbers and draw corresponding the characters starting at point x y"
  (loop for position-in-list in indexes
       for i from 1
       do (multiple-value-bind (row column)
	      (floor position-in-list 16)
	    (draw-img font-image
		      renderer
		      :x (+ (* i 8) x)
		      :y y
		      :w 8
		      :h 8
		      :start-x (* 8 column)
		      :start-y (* 8 row)
		      :end-x 8
		      :end-y 8))))

(defun draw-menu (renderer font-image)
  "Draw the main pause menu"
  (draw-char (string-to-index "POK^DEX") renderer font-image 138 8)
  (draw-char (string-to-index "POK^MON") renderer font-image 138 24)
  (draw-char (string-to-index "PACK")    renderer font-image 138 40)
  (draw-char (string-to-index "{|GEAR")  renderer font-image 138 56)
  (draw-char (string-to-index "PLAYER")  renderer font-image 138 72)
  (draw-char (string-to-index "SAVE")    renderer font-image 138 88)
  (draw-char (string-to-index "OPTION")  renderer font-image 138 104)
  (draw-char (string-to-index "EXIT")    renderer font-image 138 120)
  (draw-char (string-to-index "~") renderer font-image 130 *y*))
