LISP ?= sbcl

PREFIX ?= /usr/local

SOURCES=$(wildcard *.lisp)
TARGET=src/ur-game

default build all: $(TARGET)

$(TARGET): $(SOURCES)
	$(LISP) --load ur-game.asd \
			--eval '(ql:quickload :ur-game)' \
			--eval '(asdf:make :ur-game)' \
			--eval '(quit)'

clean:
	$(RM) $(TARGET)

.PHONY: default build all clean
