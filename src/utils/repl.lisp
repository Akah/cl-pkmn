(in-package #:pkmn)

(defun repl ()
  (format t "~s " 'PKMN-REPL>)
  (format t "~a~%" (eval (read)))
  (repl))
