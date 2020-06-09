build:
	sbcl --load pkmn.asd \
		--eval '(ql:quickload :pkmn)' \
		--eval '(asdf:make :pkmn)' \
		--eval '(quit)'

clean:
	rm -f pkmn
	rm -rf ./*.fasl
