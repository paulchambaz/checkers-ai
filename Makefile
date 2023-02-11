checkers-ai: clean
	@sbcl --non-interactive \
				--load checkers-ai.asd \
		    --eval '(ql:quickload :checkers-ai)' \
				--eval '(asdf:make :checkers-ai)'

run:
	@sbcl --load checkers-ai.asd \
		    --eval '(ql:quickload :checkers-ai)' \
				--eval '(checkers-ai:main)'

test:
	@sbcl --non-interactive \
				--load checkers-ai.asd \
				--eval '(ql:quickload :checkers-ai)' \
				--eval '(asdf:test-system :checkers-ai)'

clean:
	@rm -f checkers-ai
