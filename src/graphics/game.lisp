;;;; game.lisp

;;; TODO:
;; * Seperate functions into files i.e input handling, main game loop, drawing
;;   functions, loading files.
;; * Correct game loop

(in-package #:pkmn)

(defparameter *scale* 2)
(defparameter *width*  (* 13 16 *scale*))
(defparameter *height* (* 11 16 *scale*))
(defparameter *tile-size* (* *scale* 16))

(defparameter *x* 0)
(defparameter *y* 0)

(defparameter block-time 0)

(defparameter font-image nil)

(defun render-clear (renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 0)
  (sdl2:render-clear renderer))

(defun draw (renderer)
  (sdl2:set-render-draw-color renderer 255 0 0 0)
  (sdl2:render-fill-rect
   renderer (sdl2:make-rect *x* *y* *tile-size* *tile-size*)))

(defun draw-player-stat-holder (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 0)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 300 300 (* *scale* 50) *scale*))
  (sdl2:render-fill-rect renderer (sdl2:make-rect 306 297 9 3))
  (sdl2:render-fill-rect renderer (sdl2:make-rect 312 294 9 6))
  (sdl2:render-fill-rect renderer (sdl2:make-rect 318 291 9 9))
  (sdl2:render-fill-rect renderer (sdl2:make-rect 438 291 15 9))
  (sdl2:set-render-draw-color renderer 70 100 225 0)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 388 291 50 3)))

(defmacro with-image-init (&body body)
  `(progn
    (sdl2-image:init '(:png))
    (unwind-protect
	 (progn
	   ,@body))
    (sdl2-image:quit)))

(defun scale (num)
  (* *scale* num))

(defun print-debug (level string)
  (format t "[~a] ~a~%" level string))

(defun load-and-draw-img (src x y renderer)
  (let* ((image (sdl2-image:load-image src))
	 (width (sdl2:surface-width image))
	 (height (sdl2:surface-height image))
	 (dst-rect (sdl2:make-rect x y (scale width) (scale height)))
	 (texture (sdl2:create-texture-from-surface renderer image)))
    (sdl2:render-copy renderer texture :dest-rect dst-rect)
    (sdl2:destroy-texture texture)
    (sdl2:free-rect dst-rect)))

(defun load-media ()
  (let* ((stem "/home/rob/quicklisp/local-projects/cl-pkmn/res/img/")
	 (font (concatenate 'string stem "font-v2.png")))
    (setf font-image (sdl2-image:load-image font))
    (print-debug :info (format nil "Loaded file: ~a" font))))

(defun draw-img (img x y renderer window)
  (let* ((width (sdl2:surface-width img))
	 (height (sdl2:surface-height img))
	 (dst-rect (sdl2:make-rect x y (scale width) (scale height))))
    (sdl2:blit-surface img
		       (sdl2:make-rect 16 0 8 8)
		       (sdl2:get-window-surface window)
		       (sdl2:make-rect 0 0 8 8))
    (sdl2:free-rect dst-rect)))

(defun main-loop (renderer window)
  "main game loop called in init environment"
  ;(render-clear renderer)
  ;;
  (draw-img font-image 32 0 renderer window)
  ;;
  (sdl2:render-present renderer)
  (when (< 0 block-time)
    (decf block-time))
  (sdl2:delay 10)) ;; replace with proper game loop timer

(let ((paused nil))
  (defun toggle-pause ()
    (setq paused (not paused))
    paused)
  (defun value-pause ()
    paused))

(defun handle-key (keysym)
  "take an input and map to output for the key"
  (case (sdl2:scancode keysym)
    (:scancode-escape (sdl2:push-event :quit))
    (:scancode-grave  (toggle-pause)))
  (unless (> block-time 1)
    (setf block-time 30) ;; ms?
    (case (sdl2:scancode keysym)
      (:scancode-left  (setq *x* (- *x* *tile-size*)))
      (:scancode-right (setq *x* (+ *x* *tile-size*)))
      (:scancode-up    (setq *y* (- *y* *tile-size*)))
      (:scancode-down  (setq *y* (+ *y* *tile-size*))))))

(defun init ()
  "initilise gui and start main loop"
  (write-line "init game")
  (sdl2:with-init (:everything)
    (sdl2:with-window (window
		       :title "PKMN"
		       :w *width*
		       :h *height*
		       :x (- 1353 *width*)
		       :y 37;(- 1080 *height*)
		       :flags '(:shown))
      (sdl2:with-renderer (renderer
			   window
			   :flags '(:accelerated))
	(with-image-init
	  ;; after inits, code to be run before loop starts
	  (load-media)
	  (sdl2:with-event-loop (:method :poll)
          (:keydown
           (:keysym keysym)
	   (handle-key keysym))
          (:idle
	   ()
	   (unless (value-pause)
	     (main-loop window renderer)))
          (:quit
	   ()
	   (sdl2-image:quit)
	   t)))))))
        
