;;;; font.lisp

(in-package #:pkmn)

;; f(string) => [1D coordinates matching font image]
;; * take string input
;; * foreach char get location on image
;; * return list of char-locations with source as render-items to be renderered
;;   in renderer.
;; 33 means space is -1, not shown in text image but when -1; need to remember
;; to leave a space of x pixels

(defun string-to-index (string)
  "convert string to list of ascii characters shifted by 32 
   (index of font-image)"
  (mapcar (lambda (char) (- (char-code char) 32))
   	  (loop for x from 1 to (length string)
   	     collect (character (subseq string (1- x) x)))))

