include config.mk

SRC = ../source/*.c

build: clean
	@${CC} -o ../checkers-ai ${CFLAGS} ${LDFLAGS} ${SRC}

release: clean
	@${CC} -o ../checkers-ai ${INSTALL_CFLAGS} ${LDFLAGS} ${SRC}
	@${STRIP} ../checkers-ai

debug: clean
	@${CC} -o ../debug ${DEBUG_CFLAGS} ${LDFLAGS} ${SRC}

clean:
	@rm -f ../checkers-ai ../debug
