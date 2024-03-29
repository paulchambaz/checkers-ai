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

compute-database:
	@sbcl --dynamic-space-size 12288 \
		--load checkers-ai.asd \
		--eval '(ql:quickload :checkers-ai)' \
		--eval '(checkers-ai::endgame-compute $(piece) "data/endgame-database.csv")' \
		--eval '(exit)'

genetic-algorithm:
	@sbcl --dynamic-space-size 12288 \
		--load checkers-ai.asd \
		--eval '(ql:quickload :checkers-ai)' \
		--eval '(checkers-ai::evolution-step (checkers-ai::init-gen) 0)' \
		--eval '(exit)'

average-ai:
	@sbcl --dynamic-space-size 12288 \
		--load checkers-ai.asd \
		--eval '(ql:quickload :checkers-ai)' \
		--eval '(checkers-ai::average-ai "data/hard.csv" "data/average-generation.csv")' \
		--eval '(exit)'
