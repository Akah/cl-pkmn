;;;; game.lisp

(in-package #:pkmn)

(defparameter *scale* 3)
(defparameter *width*  (* 13 16 *scale*))
(defparameter *height* (* 11 16 *scale*))

(defparameter *x* 0)
(defparameter *y* 0)

(defun test-render-clear (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun test-render-fill-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl2:rects*
             (loop :for x :upto 5
                :collect (sdl2:make-rect (+ 500 (* x 10)) 400 8 8)))
    (sdl2:set-render-draw-color renderer 255 0 255 255)
    (sdl2:render-fill-rects renderer rects num)))

(defun draw (renderer)
  (sdl2:set-render-draw-color renderer 255 0 0 0)
  (sdl2:render-fill-rect renderer (sdl2:make-rect *x* *y* 16 16)))

(defun draw-img (renderer)
  (sdl2-image:init '(:png))
  
  (let* ((image (sdl2-image:load-image "espeon-front.png"))
	 (dst-rect (sdl2:make-rect 5 5 200 200))
	 (texture (sdl2:create-texture-from-surface renderer image)))
    (sdl2:render-copy renderer texture
		      :dest-rect dst-rect))
  
  (sdl2-image:quit))

(defun main-loop (renderer win)
  "main game loop called in init environment"
  (test-render-clear renderer)
  ;;
  (draw renderer)
  (draw-img renderer)
  ;;
  (sdl2:render-present renderer)
  (sdl2:delay 10))

(defun handle-key (keysym)
  "take an input and map to output for the key"
  (format t "Key pressed: ~s~%" (sdl2:scancode-value keysym))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
    (sdl2:push-event :quit))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
    (setq *x* (- *x* 10)))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
    (setq *x* (+ 10 *x*)))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-up)
    (setq *y* (- *y* 10)))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-down)
    (setq *y* (+ 10 *y*))))

(defun init ()
  "initilise gui and start main loop"
  (sdl2:with-init (:everything)
    (sdl2:with-window (win
		       :title "CL - PKMN"
		       :w *width*
		       :h *height*
		       :x (- 1910 *width*)
		       :y 80;(- 1080 *height*)
		       :flags '(:shown))
      (sdl2:with-renderer (renderer
			   win
			   :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
	   (handle-key keysym))
          (:idle () (main-loop renderer win))
          (:quit () t))))))
