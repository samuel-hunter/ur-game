LISP ?= sbcl

PREFIX ?= /usr/local

SOURCES=$(wildcard *.lisp)
TARGET=ur-game

default build all: $(TARGET)

$(TARGET): $(SOURCES)
	$(LISP) --load ur-game.asd \
			--eval '(ql:quickload :ur-game)' \
			--eval '(asdf:make :ur-game)' \
			--eval '(quit)'
	mv src/$(TARGET) $(TARGET)

clean:
	$(RM) $(TARGET)

.PHONY: default build all clean
