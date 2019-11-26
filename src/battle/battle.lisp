(in-package #:pkmn)

;;===================================PRINT==========================================

(defun print-battle-status (player enemy)
  (write-line "--------------------------------------------------")
  (format t "~% ~A: ~Dhp"
	  (pkmn-name player)
	  (stats-hp (pkmn-stats player)))
  (format t "~% ~A: ~Dhp"
	  (pkmn-name enemy)
	  (stats-hp (pkmn-stats enemy))))

(defun print-options ()
  (format t "~%~% What should you do?~% FIGHT    BAG~% POKéMON  RUN~%"))

(defun print-moves (pokemon)
  (format t "~{ ~a~^   ~a~%~}"
	  (mapcar #'(lambda (lst) (move-name (symbol-value lst)))
		  (pkmn-moves pokemon))))

(defun print-win (winner)
  (format  t "~a wins" winner))

;;=================================BOOLEAN-CHECKS===================================

(defun is-alive (pkmn)
  (> (stats-hp (pkmn-stats pkmn)) 0))

(defun is-hit (attacker defender move)
  (let ((threshold (* (move-accuracy move)
		      (stats-accuracy (pkmn-stats attacker))
		      (stats-evasiveness (pkmn-stats defender))))
	(rand (+ 1 (random 255))))
    (<= rand threshold)))

(defun is-critical (speed)
  (let ((threshold (/ 2 speed))
       (rand (+ 1 (random 255))))
    (<= rand threshold)))

;;===================================================================================

(defun calc-damage (attacker defender move)
  (let* ((effectiveness (get-effectiveness move defender))
	 (level (pkmn-level attacker))
	 (attack)
	 (defence)
	 (multiplier effectiveness))
    ;; message output of effectiveness
    (format t "effectiveness: ~a~%" effectiveness)
    (case effectiveness
      (0  (format t "~a doesn't effect ~a~%" move (pkmn-name defender)))
      (5  (format t "~a is not very effective~%" move))
      (20 (format t "~a is super effective~%" move)))
    ;; set atk and def to match move category
    (if (eq (move-category move) 'physical)
	(progn ; physical
	  (setq attack  (stats-atk (pkmn-stats attacker)))
	  (setq defence (stats-def (pkmn-stats defender))))
	(progn ; special
	  (setq attack  (stats-sp-atk (pkmn-stats attacker)))
	  (setq defence (stats-sp-def (pkmn-stats defender)))))
    (if (is-critical (stats-speed (pkmn-stats attacker)))
	(setq multiplier (* 2 multiplier)))

    ;; see: https://bulbapedia.bulbagarden.net/wiki/Damage
    ;; todo: modifier is more than just effectiveness. see link above.
    ;; (((((2 * level) / 5) + 2) * power * attack) / defence / (50 + 2)) * effectiveness)
    (* (+ (/ (* (* (+ (/ (* 2 level) 5) 2) (move-power move)) (/ attack defence)) 50) 2) multiplier)))

(defun fight (player enemy)
  (let ((input)
	(move))
    (write-line "Select move (0,1,2,3)")
    (print-moves player)
    (setq input (read))
    ;; set input 0 1 2 or 3 then call based on that
    (setq move (symbol-value (nth input (pkmn-moves player))))
    (format t "~% ~a used ~a~%" (pkmn-name player) (move-name move))
    (if (is-hit player enemy move)
	(progn
	  ;; set hp to be hp - calculated damage
	  (setf (stats-hp (pkmn-stats enemy))
		;; truncate = round always down (floor)
		(truncate (- (stats-hp (pkmn-stats enemy))
			  (calc-damage player enemy move))))
	  (funcall (move-effect move)))
	(write-line "attack missed"))))
  
(defun bag ()
  (write-line "chose bag"))

(defun pokemon ()
  (write-line "chose pokemon"))

(defun run()
  (write-line "chose run"))

(defun get-enemy-option ()
  )

(defun battle (player enemy &optional (loop-count 0))
  (let ((player-option)
	(enemy-option))
    (print-battle-status player enemy)
    (print-options)
    (setq player-option (read))
    (setq enemy-option (get-enemy-option))
    (case player-option
      (fight   (fight player enemy))
      (bag     (bag))      ;does nothing
      (pokemon (pokemon))  ;does nothing
      (run     (run))      ;does nothing
      (t       (write-line "invalid input")))
    (incf loop-count)
    (if (and (is-alive player)
	     (is-alive enemy))
	(battle player enemy loop-count)
	;;else
	(if (is-alive player)
	    (format t "~a wins~%" player)
	    ;;else
	    (format t "~a wins~%" enemy)))))
  
  
