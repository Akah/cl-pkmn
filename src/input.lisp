(in-package #:pkmn)

(defun handle-key-start-menu ())

(defun handle-key-overworld ())

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

(defun handle-key (keysym state)
  "take an input and map to output for the key"

  (case (sdl2:scancode keysym)
    (:scancode-escape (sdl2:push-event :quit));; swap with menu
    (:scancode-grave  (toggle-pause)); commandline
    (:scancode-return (print-debug :info "return pressed"))
    (otherwise
     (case state
       (:start-menu (handle-key-start-menu))
       (:overworld (handle-key-overworld))
       (:menu (handle-key-menu keysym))
       (:battle (handle-key-battle))))))

      ;; normal player movement
      ;; (:scancode-left  (setq *x* (- *x* *tile-size*)))
      ;; (:scancode-right (setq *x* (+ *x* *tile-size*)))
      ;; (:scancode-up    (setq *y* (- *y* *tile-size*)))
      ;; (:scancode-down  (setq *y* (+ *y* *tile-size*))))))
