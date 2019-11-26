;;;; pkmn.lisp

(in-package #:pkmn)

;;; have both players parties and bags(inventory) as global

(defparameter *tackle* (make-move
			:name "tackle"
			:type 'normal
			:pp-current 35
			:pp-max 35
			:accuracy 100
			:power 40
			:category 'physical
			:effect (lambda ())))
(defparameter *test* (make-move
		      :name "test"
		      :type 'bug;super effective against dark
		      :pp-current 10
		      :pp-max 10
		      :accuracy 100
		      :power 60
		      :category 'special
		      :effect (lambda ())))

;; needs to be moved to tm.lisp
(defun tackle (pp)
  (make-move
   :name "tackle"
   :type 'normal
   :pp-current pp
   :accuracy 100
   :power 40
   :category 'physical
   :effect nil))

(defparameter *espeon* (make-pkmn
			:name "ESPEON"
			:stats (make-stats
				:hp 65
				:atk 65
				:def 60
				:sp-atk 130
				:sp-def 95
				:speed 110
				:accuracy 100
				:evasiveness 1)
			:type 'psychic
			:level 50
			:moves '(*tackle* *test*)))

(defparameter *umbreon* (make-pkmn
			 :name "UMBREON"
			 :stats (make-stats
				 :hp 95
				 :atk 65
				 :def 110
				 :sp-atk 60
				 :sp-def 130
				 :speed 65
				 :accuracy 100
				 :evasiveness 1)
			 :type 'dark
			 :level 50
			 :moves '(*tackle*)))

(defun start ()
  (let ((player *espeon*)
	(enemy *umbreon*))
    (setf (stats-hp (pkmn-stats *espeon*)) 65)
    (setf (stats-hp (pkmn-stats *umbreon*)) 95)
    (battle player enemy)))
