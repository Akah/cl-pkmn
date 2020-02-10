;;;; font.lisp

(in-package #:pkmn)

;; f(string) => [render-item]
;; take string input
;; foreach char get location on image
;; return list of char-locations with source as render-items to be renderered in renderer

(defun split-string (string)
  (loop for x from 1 to (length string)
     collect (subseq string (1- x) x)))

(defun get-coordinates (char)
  (cond ((string= char "A") 0)
	((string= char "B") 0)
	((string= char "C") 0)
	((string= char "D") 0)
	((string= char "E") 0)
	((string= char "F") 0)))

;;(defun as-text (string)
;;  (let ((chars (split-string string)))))
