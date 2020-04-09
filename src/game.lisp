(in-package #:pkmn)

(defparameter *scale* 3)
(defparameter *tile-width* 16)
(defparameter *screen-tile-width* 13)
(defparameter *screen-tile-height* 11)
(defparameter *screen-pixel-width*
  (* *screen-tile-width* *tile-width* *scale*))
(defparameter *screen-pixel-height*
  (* *screen-tile-width* *tile-width* *scale*))

(defmacro with-image-init (&body body)
  `(progn
    (sdl2-image:init '(:png))
    (unwind-protect
	 (progn
	   ,@body))
    (sdl2-image:quit)))

(defun handle-key (keysym)
  "take an input and map to output for the key pressed"
  (format t "Key pressed: ~s~%" (sdl2:scancode-value keysym))
  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
    (sdl2:push-event :quit)))

(defun main-loop (renderer)
  (format t "loop")
  (clear-screen renderer)
  (let ((rect (make-rect 0 0 *screen-pixel-width* *screen-pixel-height*)))
    (draw-rect rect 0 0 0 renderer)))

(defun init-game ()
  "initilise cffi: sdl2 and sdl2-image environments"
  (sdl2:with-init (:everything)
    (sdl2:with-window (win
		       :title "pkmn"
		       :w *screen-pixel-width*
		       :h *screen-pixel-height*
		       :x (- 1910 *screen-pixel-width*)
		       :y 80
		       :flags '(:shown))
      (sdl2:with-renderer (renderer
			   win
			   :flags '(:accelerated))
	(with-image-init
	  (sdl2:with-event-loop (:method :poll)
	    (:keyup
	     (:keysym keysym)
	     (handle-key keysym))
	    (:idle
	     ()
	     ;(unless (value-pause)
	       (main-loop renderer))
	    (:quit
	     ()
	     (sdl2-image:quit)
	     t)))))))
