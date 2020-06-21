(in-package #:pkmn)

(defun handle-key-start-menu ()
  "press any key to move past start menu"
  (setf *state* :overworld)
  (print-debug :debug (format nil "set state to ~a" *state*)))

(defun handle-key-overworld (keysym)
  (unless (> block-time 1)
    (setf block-time 20)
    (case (sdl2:scancode keysym)
      (:scancode-left
       (progn
	 (setf (player-x player) (- (player-x player) 16))
	 (setf (player-direction player) :left)))
      (:scancode-right
       (progn
	 (setf (player-x player) (+ (player-x player) 16))
	 (setf (player-direction player) :right)));; need to flip
      (:scancode-up
       (progn
	 (setf (player-y player) (- (player-y player) 16))
	 (setf (player-direction player) :up)))
      (:scancode-down
       (progn
	 (setf (player-y player) (+ (player-y player) 16))
	 (setf (player-direction player) :down))))))

;; move to player file
(defun pos-from-direction (player)
  (case (player-direction player)
    (:left 2)
    (:right 2)
    (:up 1)
    (:down 0)))

(defun should-flip (player)
  (if (eq (player-direction player) :right) 1 0))

(defun handle-key-menu (keysym)
  (unless (> block-time 1)
    (setf block-time 20) ;; ms?
    (print-debug :info (format nil "pressed ~a y-pos: ~a" (sdl2:scancode keysym) *y*))
    (case (sdl2:scancode keysym)
      (:scancode-left  (setq *x* (- *x* 0)))
      (:scancode-right (setq *x* (+ *x* 0)))
      (:scancode-up    (setq *y* (- *y* 16)))
      (:scancode-down  (setq *y* (+ *y* 16))))))

(defun handle-key-battle ())

(defun handle-key (keysym)
  "take an input and map to output for the key"

  (case (sdl2:scancode keysym)
    (:scancode-escape (sdl2:push-event :quit));; swap with menu
    (:scancode-grave  (toggle-pause)); commandline
    (:scancode-return (print-debug :info "return pressed"))
    (otherwise
     (case *state*
       (:start-menu (handle-key-start-menu))
       (:overworld (handle-key-overworld keysym))
       (:menu (handle-key-menu keysym))
       (:battle (handle-key-battle))))))
