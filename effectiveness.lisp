(in-package #:pkmn)

;;make assoc-array of types numbered
;;number represents order in the array of effectivenesses

(defparameter *types* '(normal fire water electric grass ice fighting poison flying
			psychic bug rock ghost dragon dark steel))

(defparameter *table* '(;no fr wa el gr ic fg po gd fl ps bg ro gh dr da st
			(10 10 10 10 10 10 10 10 10 10 10 10  5  0 10 10  5)  ;normal
			(10  5  5 10 20 20 10 10 10 10 10 20  5 10  5 10 20)  ;fire
			(10 20  5 10  5 10 10 10 20 10 10 10 20 10  5 10 10)  ;water
			(10 10 20  5  5 10 10 10  0 20 10 10 10 10  5 10 10)  ;electric
			(10  5 20 10  5 10 10  5 20  5 10  5 20 10  5 10  5)  ;grass
			(10  5  5 10 20  5 10 10 20 20 10 10 10 10 20 10  5)  ;ice
			(20 10 10 10 10 20 10  5 10  5  5  5 20  0 10 20 20)  ;fighting
			(10 10 10 10 20 10 10  5  5 10 10 10  5  5 10 10  0)  ;poison
			(10 20 10 20  5 10 10  5 10  0 10  5 20 10 10 10 20)  ;ground
			(10 10 10  5 20 10 20 20 10 10 10 20  5 10 10 10  5)  ;flying
			(10 10 10 10 10 10 20 20 10 10  5 10 10 10 10  0  5)  ;psychic
			(10  5 10 10 20 10  5  5 10  5 20 10 10  5 10 20  5)  ;bug
			(10 20 10 10 10 20  5 10  5 20 10 20 10 10 10 10  5)  ;rock
			( 0 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10)  ;ghost
			(10 10 10 10 10 10 10 10 10 10 10 10 10 10 20 10  5)  ;dragon
			( 0 10 10 10 10 10 10 10 10 10 20 10 10 20 10  5 10)  ;dark
			(10  5  5  5 10 20 10 10 10 10 10 10 20 10 10 10  5)));steel

;; should become a macro to return from n nested lists
(defun nested (x y lst)
  (nth x (nth y lst)))

(defun get-effectiveness (move defender)
  "return a value of 0, 0.5, 1, 2 based on effectiveness of move used on defender"
  (/ (nested
      (position (move-type move) *types*)
      (position (pkmn-type defender) *types*)
      *table*) 10)) 
