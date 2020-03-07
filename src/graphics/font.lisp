;;;; font.lisp

(in-package #:pkmn)

;; f(string) => [render-item]
;; take string input
;; foreach char get location on image
;; return list of char-locations with source as render-items to be renderered in renderer

(defun split-string (string)
  (loop for x from 1 to (length string)
     collect (subseq string (1- x) x)))

(defun get-coordinate (char)
  (char-code (character char)))

(defun get-coordinates (chars)
  (mapcar (lambda (char) (get-coordinate char)) chars))

(defun string-to-coordinates (string)
  (get-coordinates (split-string string)))
