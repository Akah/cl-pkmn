;;; view-state
;; code related to game state management

(in-package #:pkmn)

(deftype state ()
  "posible game states"
  '(member
    :start-menu
    :overworld
    :menu
    :battle))

(defun statep (value)
  "check whether value is of type state"
  (typep value 'state))

(defun do-start-menu ()
  (sb-thread:make-thread
   (lambda ()
     )
   :name "start-menu"))

(defun handle-state (state)
  "do something based on current state"
  (case state
    (:start-menu (do-start-menu))
    (otherwise (print-debug :error "state type doesn't exist: sumn dun goofd"))))
