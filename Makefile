LISP ?= sbcl

PREFIX ?= /usr/local

SOURCES=$(wildcard *.lisp)

default: ur-game
build: ur-game
all: default

ur-game: $(SOURCES)
	$(LISP) --load ur-game.asd \
			--eval '(ql:quickload :ur-game)' \
			--eval '(asdf:make :ur-game)' \
			--eval '(quit)'

install:
	install -t $(PREFIX)/bin kablature

clean:
	$(RM) kablature
	$(RM) $(EXAMPLE_TARGETS)

.PHONY: default build all install clean
