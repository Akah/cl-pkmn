;;;; font.lisp

(in-package #:pkmn)

;; f(string) => [render-item]
;; take string input
;; foreach char get location on image
;; return list of char-locations with source as render-items to be renderered in renderer

(defun string-to-coordinates (string)
  "convert string to list of ascii characters"
  (mapcar (lambda (char) (char-code char)
	     (loop for x from 1 to (length string)
		collect (character (subseq string (1- x) x))))))
