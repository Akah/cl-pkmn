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
(defparameter *y* 8)

(defparameter block-time 0)

(defparameter font-image nil)

(defparameter menu-options '("pokedex"
			     "pokemon"
			     "pack"
			     "pokegear"
			     "player"
			     "save"
			     "option"
			     "exit"))

(defmacro with-image-init (&body body)
  `(progn
    (sdl2-image:init '(:png))
    (unwind-protect
	 (progn
	   ,@body))
    (sdl2-image:quit)))

;; TODO: move into utils file/package
(defun any-nil (&rest values)
  (some #'(lambda (values) (null values)) values))

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
  (let* ((resources "/home/rob/quicklisp/local-projects/cl-pkmn/res/")
	 (images "/img/")
	 (font (concatenate 'string resources images"font-v3-4.png")))
    (setf font-image (sdl2-image:load-image font))
    (if (eq font-image nil)
	(print-debug :error (format nil "Failed to load file: ~a" font))
	(print-debug :info (format nil "Loaded file: ~a" font)))))

(defun main-loop (renderer)
  "main game loop called in init environment"
  (render-clear renderer)  
  ;;
  (draw-menu renderer font-image)
  ;;
  (sdl2:render-present renderer)
  (when (< 0 block-time)
    (decf block-time))
  ;;
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
    (:scancode-escape (sdl2:push-event :quit));; swap with menu
    (:scancode-grave  (toggle-pause)); commandline
    (:scancode-return (print-debug :info "return pressed")))
  (unless (> block-time 1)
    (setf block-time 20) ;; ms?
    (print-debug :info (format nil "pressed ~a y-pos: ~a" (sdl2:scancode keysym) *y*))
    (case (sdl2:scancode keysym)
      (:scancode-left  (setq *x* (- *x* 0)))
      (:scancode-right (setq *x* (+ *x* 0)))
      (:scancode-up    (setq *y* (- *y* 16)))
      (:scancode-down  (setq *y* (+ *y* 16))))))
      ;; normal player movement
      ;; (:scancode-left  (setq *x* (- *x* *tile-size*)))
      ;; (:scancode-right (setq *x* (+ *x* *tile-size*)))
      ;; (:scancode-up    (setq *y* (- *y* *tile-size*)))
      ;; (:scancode-down  (setq *y* (+ *y* *tile-size*))))))

(defun init ()
  "initilise gui and start main loop as a thread called sdl2-init"
  (sb-thread:make-thread
   (lambda ()
     (sdl2:with-init (:video)
       (print-debug :info "Initialising sdl2")
       (sdl2:with-window (window
			  :title "PKMN"
			  :w *width*
			  :h *height*
			  :x (- 1353 *width*)
			  :y 37;(- 1080 *height*)
			  :flags '(:shown))
	 (print-debug :info "Initialising sdl2-window")
	 (sdl2:with-renderer (renderer
			      window
			     :flags '(:accelerated))
	   
	   (print-debug :info "Initialising sdl2-renderer")
	   (with-image-init
	     (print-debug :info "Initialising sdl2-image")
	     ;; after inits, code to be run before loop starts
	     (load-media)
	     (sdl2:with-event-loop (:method :poll)
	       (:keydown (:keysym keysym)
			 (handle-key keysym))
	       (:idle ()
		      (unless (value-pause)
			(main-loop renderer)))
	       (:quit ()
		      (progn
			(print-debug :info "Quitting sdl2-image")
			(print-debug :info "Closing window"))
		      t)))))))
   :name "sdl2-init"))
