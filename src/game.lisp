;; to be made again using sdl2

(defun start-game ()
  "initialise window and start game-loop"
  (sdl2:with-init (:everything)
    (sdl2:with-window (win
		       :title "cl-pkmn"
		       :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(accelerated))
	(sdl2:with-event-loop (:method :poll)
	  (:keyup
	   (:keysym keysym)
	   (when
	       (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape) ; quit on escape
	     (sdl2:push-event :quit)))
	  (:idle ()
		 ;;do functions here
		 (sdl2:delay 10))
	  (:quit () t))))))
