; SLIME 2.24
CL-USER> (ql:quickload "pkmn")
To load "pkmn":
  Load 1 ASDF system:
    pkmn
; Loading "pkmn"
..............................
("pkmn")
CL-USER> (in-package #:pkmn)
#<PACKAGE "PKMN">
; compiling file "/home/rob/quicklisp/local-projects/cl-pkmn/src/game.lisp" (written 18 JAN 2020 12:17:57 PM):
; 
; caught ERROR:
;   READ error during COMPILE-FILE:
;   
;     Package SDL2 does not exist.
;   
;       Line: 4, Column: 29, File-Position: 96
;   
;       Stream: #<SB-INT:FORM-TRACKING-STREAM for "file /home/rob/quicklisp/local-projects/cl-pkmn/src/game.lisp" {10030EF623}>
; 
; compilation unit aborted
;   caught 1 fatal ERROR condition
;   caught 1 ERROR condition

; compilation aborted after 0:00:00.007
; compiling (ASDF/PARSE-DEFSYSTEM:DEFSYSTEM #:PKMN ...)
PKMN> (rendere)