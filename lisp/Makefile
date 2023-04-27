run:
	@sbcl --load checkers-ai.asd \
		    --eval '(ql:quickload :checkers-ai)' \
				--eval '(checkers-ai:main)' \
				--eval '(exit)'

build: clean
	@sbcl --non-interactive \
				--load checkers-ai.asd \
		    --eval '(ql:quickload :checkers-ai)' \
				--eval '(asdf:make :checkers-ai)'

test:
	@sbcl --non-interactive \
				--load checkers-ai.asd \
				--eval '(ql:quickload :checkers-ai)' \
				--eval '(asdf:test-system :checkers-ai)'

clean:
	@rm -f checkers-ai

debug:
	@sbcl --dynamic-space-size 4096 \
				--load checkers-ai.asd \
		    --eval '(ql:quickload :checkers-ai)'

