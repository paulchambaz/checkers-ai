c: clean
	@make -C c/build build
	@mv c/checkers-ai release/c/checkers-ai
	@cp -r c/data release/c/data

lisp: clean
	@make -C lisp build
	@mv lisp/checkers-ai release/lisp/checkers-ai
	@cp -r lisp/data release/lisp/data

paper: clean
	@make -C paper doc

clean:

full-clean:
	@rm -fr release/c/checkers-ai
	@rm -fr release/c/data
	@rm -fr release/lisp/checkers-ai
	@rm -fr release/lisp/data
