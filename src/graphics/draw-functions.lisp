(in-package #:pkmn)

(defun clear-screen (renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 0)
  (sdl2:render-clear renderer))

(defun make-rect (x y w h)
  (sdl2:make-rect x y w h))

(defun draw-rect (rect r g b renderer)
  (sdl2:set-render-draw-color renderer r g b 0)
  (sdl2:render-fill-rect renderer rect))

(defun make-texture (src renderer)
  (sdl2:create-texture-from-surface renderer (sdl2-image:load-image src)))

(defun scale (value)
  (* *scale* value))

(defun draw-texture (texture pos-x pos-y renderer)
  (let* ((width (sdl2:texture-width texture))
	 (height (sdl2:texture-height texture))
	 (dst-rect (make-rect pos-x posy (scale width) (scale height))))
    (sdl2:render-copy renderer texture :dest-rect dst-rect)))
