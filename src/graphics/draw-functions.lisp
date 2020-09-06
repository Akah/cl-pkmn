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

(defun draw-rect (renderer x y w h)
  "draw rectangle ")

(defun draw-box (renderer x y w h)
  "draw box from start (upper-left) to end (lower-right)"
  
  (sdl2:set-render-draw-color renderer 255 0 0 0)
  (sdl2:render-fill-rect renderer (sdl2:make-rect x y w h))
  (sdl2:set-render-draw-color renderer 0 0 0 0)
  (sdl2:render-fill-rect renderer (sdl2:make-rect (+ 4 x) y (- w 8) 2))
  (sdl2:render-fill-rect renderer (sdl2:make-rect (+ 2 x) (+ 2 y) 2 2))
  (sdl2:render-fill-rect renderer (sdl2:make-rect x (+ 4 y) 2 (- h 8)))
  (sdl2:render-fill-rect renderer (sdl2:make-rect (+ 2 x) (+ y (- h 4)) 2 2))
  (sdl2:render-fill-rect renderer (sdl2:make-rect (+ 4 x) (+ 8 h) (- w 8) 2))
  (sdl2:render-fill-rect renderer (sdl2:make-rect (+ x (- w 4)) (+ 6 h) 2 2))
  (sdl2:render-fill-rect renderer (sdl2:make-rect (+ x (- w 4)) (+ 2 y) 2 2))
  (sdl2:render-fill-rect renderer (sdl2:make-rect (+ x (- w 2)) (+ 4 y) 2 (- h 8))))

;; TODO: allow variables otherthan x & y to be optional
(defun draw-img (img renderer &key x y w h start-x start-y end-x end-y flip)
  "draw image from xy to wh with optional start and end for cropping"
  (unless flip (setf flip 0))
  (let* ((texture (sdl2:create-texture-from-surface renderer img))
	 (src-rect (sdl2:make-rect start-x start-y end-x end-y))
	 (dst-rect (sdl2:make-rect (scale x) (scale y) (scale w) (scale h))))
    (sdl2:render-copy-ex renderer texture
			 :source-rect src-rect
			 :dest-rect dst-rect
			 :flip (list flip))
    (sdl2:destroy-texture texture)
    (sdl2:free-rect src-rect)
    (sdl2:free-rect dst-rect)))

(defun draw-char (indexes renderer font-image x y)
  "Take list of numbers and draw corresponding the characters starting at point x y"
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

(defun get-image (symbol-name)
  (cdr (assoc symbol-name images)))

(defun check-asset (file-name symbol)
  "load image asset when undefined"
  (when (null (get-image symbol))
    (load-media file-name symbol)))

(defvar *start-menu-state* :menu)

(defun draw-start-menu (renderer)
  (check-asset "img/pokemon.png" 'start-image)
  (check-asset "img/font-v3-4.png" 'font-image)
  (draw-img (get-image 'start-image) renderer
	    :x 33 :y 35 :w 128 :h 56 :start-x 0 :start-y 0 :end-x 128 :end-y 56)
  (when (eq 0 (mod (floor (/ (sdl2:get-ticks) 750)) 2))
    (draw-text "press any key to start" renderer 8 120)))

(defun draw-player (renderer)
  (check-asset "img/player-sprites.png" 'characters-image)
  (draw-img (get-image 'characters-image) renderer
	    :x (player-x player)
	    :y (player-y player)
	    :w 16
	    :h 16
	    :start-x (* 16 (pos-from-direction player))
	    :start-y 0
	    :end-x 16
	    :end-y 16
	    :flip (should-flip player)))

(defun draw-text (string renderer x y)
  (draw-char (string-to-index string) renderer (get-image 'font-image) x y))

(defun draw-overworld (renderer)
  (draw-player renderer))

(defun draw-battle (renderer)
  ;; replace with parameter passed pokemon.
  ;; eg ""or have all pokemon loaded from one picture
  ;; (check-asset "img/espeon-back.png" 'espeon-image)
  ;; (check-asset "img/umbreon-front.png" 'umbreon-image)
  (check-asset "img/font-v3-4.png" 'font-image)
  (check-asset "img/umbreon-front.png" 'umbreon-image)
  ;; (draw-text "Umbreon" renderer 0 0)
  (draw-box renderer 30 20 200 200))
  ;; (when (null 'espeon)
  ;;   )
  ;; (let ((espeon nil))
  ;;   (load-media 'img "espeon-back.png" 'espeon)))
  ;; draw pokemans
  ;; draw status bars
  ;; draw menu

(defun draw-state (state renderer)
  (case state
    (:start-menu (draw-start-menu renderer))
    (:overworld (draw-overworld renderer))
    (:battle (draw-battle renderer))))
