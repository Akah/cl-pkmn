(in-package #:pkmn)

(defstruct pkmn
  name  ; string
  stats ;'stats
  type  ; string?
  moves ; list of 'move
  level ; int
  exp)  ; int

(defstruct stats
  ;; all ints
  hp
  atk
  def
  sp-atk
  sp-def
  speed
  evasiveness
  accuracy)

(defstruct move
  name      ; string
  type      ; string?
  pp-current; int
  pp-max    ; int
  accuracy  ; float
  power     ; int
  effect
  category)

(defstruct item
  src
  x
  y)
