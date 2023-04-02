VERSION = 1.0
INCS = -I ../include
LIBS = -lSDL2 -lm
CFLAGS = -std=c99 ${INCS} -DVERSION=\"${VERSION}\" -DNDBUG
INTALL_CFLAGS = -std=c99 ${INCS} -DVERSION=\"${VERSION}\" -DNDBUG -Ofast
DEBUG_CFLAGS = ${CFLAGS} -UNDEBUG -O0 -g -ggdb -Wall -Wextra -Wno-unused-parameter
LDFLAGS += ${LIBS}
CC ?= cc
STRIP ?= strip
