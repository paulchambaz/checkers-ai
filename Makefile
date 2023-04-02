c: clean
	@make -C c/build build
	@mv c/checkers-ai release/c
	@cp -r c/data release/c

lisp: clean
	@make -C lisp build
	@mv lisp/checkers-ai release/lisp
	@cp -r lisp/data release/lisp

paper: clean
	@make -C paper doc

clean:

full-clean:
	@rm -fr release/c/checkers-ai
	@rm -fr release/c/data
	@rm -fr release/lisp/checkers-ai
	@rm -fr release/lisp/data

build: c lisp
	@mkdir checkers-ia-bouhabei-chambaz
	@cp -r c lisp release README.md Makefile checkers-ia-bouhabei-chambaz
	@cp paper/compte_rendu.pdf checkers-ia-bouhabei-chambaz/
	@zip -r checkers-ia-bouhabei-chambaz.zip checkers-ia-bouhabei-chambaz
	@rm -fr checkers-ia-bouhabei-chambaz
