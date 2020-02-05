FROM alpine:edge AS build

# Install lisp (available from testing repo)
RUN echo "http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
RUN apk update && apk add --no-cache sbcl


# Install quicklisp
ADD https://beta.quicklisp.org/quicklisp.lisp /opt/quicklisp.lisp
RUN sbcl --load /opt/quicklisp.lisp \
	--eval '(quicklisp-quickstart:install)' \
	--eval '(ql-util:without-prompting (ql:add-to-init-file))' \
	--eval '(quit)'

# Build program
WORKDIR /src
ADD src ./src
ADD ur-game.asd ./
RUN sbcl --load ur-game.asd \
	--eval '(ql:quickload :ur-game)' \
	--eval '(asdf:make :ur-game)' \
	--eval '(quit)'

# Image to deploy
FROM alpine:edge

# Install lisp as usual
RUN echo "http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
RUN apk update && apk add --no-cache sbcl

# Set-up user
RUN adduser --disabled-password -s /bin/ash ur-game
USER ur-game

WORKDIR /srv/ur-game
COPY --chown=ur-game --from=build /src/src/ur-game ./
ADD htdocs ./htdocs

ENV PRODUCTION 1

# http
EXPOSE 8080
# wss
EXPOSE 8082

ENTRYPOINT ["./ur-game"]
