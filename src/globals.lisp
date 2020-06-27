;;;; Globals
;; dirty global variables

(in-package #:pkmn)

(defparameter *state* :start-menu)
(defparameter *scale* 2)
(defparameter *width*  (* 13 16 *scale*))
(defparameter *height* (* 11 16 *scale*))
(defparameter *title* "PKMN")
(defparameter *tile-size* (* *scale* 16))
(defparameter *x* 0)
(defparameter *y* 8)
(defparameter block-time 0)
;; images
(defparameter characters-image nil)
(defparameter font-image nil)
(defparameter start-image nil)
;; fps
(defparameter last-time 0)
(defparameter current-fps 0)
(defparameter frames 0)
(defconstant +fps-interval+ 1000)
;;
(defparameter player (make-player :x 0 :y 0 :direction :right :moving nil))
