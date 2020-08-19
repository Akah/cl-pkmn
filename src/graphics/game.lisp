;;;; game.lisp

(in-package #:pkmn)

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
     (print-debug :info "Initialising sdl2:image")
     (unwind-protect
	  (progn
	    ,@body))
     (sdl2-image:quit)
     (print-debug :info "Killing sdl2:image")))

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

(defun load-media (location symbol)
  (let ((file-string (merge-pathnames location +resource-path+)))
    (setf (symbol-value symbol) (sdl2-image:load-image file-string))
    (if (eq (symbol-value symbol) nil)
	(print-debug :error (format nil  "Failed to load file: ~a" file-string))
	(print-debug :info (format nil "Loaded file: ~a" file-string)))))

(defun main-loop (renderer window)
  "main game loop called in init environment"
  (render-clear renderer)  
  ;;
  (draw-state *state* renderer)
  ;;
  (sdl2:render-present renderer)
  (when (< 0 block-time)
    (decf block-time))
  (incf frames)
  ;;(format t "~a~%" frames)
  (when (< last-time (- (sdl2:get-ticks) +fps-interval+))
    (setf last-time (sdl2:get-ticks))
    (setf current-fps frames)
    (setf frames 0))
  (sdl2:set-window-title window (format nil "PKMN - fps: ~a" current-fps))
  ;;
  (sdl2:delay 32)) ;; replace with proper game loop timer, currently forces 30fps

(let ((paused nil))
  (defun toggle-pause ()
    (setq paused (not paused))
    paused)
  (defun value-pause ()
    paused))

(defun init ()
  "initilise gui and start main loop as a thread called sdl2-init"
  (sb-thread:make-thread
   (lambda ()
     (sdl2:with-init (:video)
       (print-debug :info "Initialising sdl2")
       (sdl2:with-window (window
			  :title *title*
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
	     ;; after inits, code to be run before loop starts
	     (sdl2:with-event-loop (:method :poll)
	       (:keydown (:keysym keysym)
			 (handle-key keysym))
	       (:idle ()
		      (unless (value-pause)
			(main-loop renderer window)))
	       (:quit ()
		      (progn
			(print-debug :info "Quitting sdl2-image")
			(print-debug :info "Closing window"))
		      t)))))))
   :name "sdl2-init"))
