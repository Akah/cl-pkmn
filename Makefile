build:
	rm -f pkmn
	sbcl --load pkmn.asd \
		--eval '(ql:quickload :pkmn)' \
		--eval '(asdf:make :pkmn)' \
		--eval '(quit)'
