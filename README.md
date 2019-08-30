# ur-game

This will be a web server that hosts 2-player sessions of [The Royal
Gameo of Ur](https://en.wikipedia.org/wiki/Royal_Game_of_Ur), inspired
by this [YouTube video](https://www.youtube.com/watch?v=WZskjLq040I)
from The British Museum:

[![Tom Scott vs Irving Finkel: The Royal Game of
Ur](https://img.youtube.com/vi/WZskjLq040I/0.jpg)](https://youtu.be/WZskjLq040I
"Tom Scott vs Irving Finkel: The Royal Game of Ur")

# Host the Software

The server is hosted using Common Lisp with Quicklisp. To run the
server, install the project folder in
`~/quicklips/local-projects/ur-game`, and then run (using SBCL):

    $ sbcl --eval '(ql:quickload :ur-game)' '(ur-game:start)'
    
By default, the server runs an HTTP socket on 8080, and an WS socket
on 8081. With the environment variable `PRODUCTION=1`, the WS socket
is moved to 8082. This is mainly used so that nginx can be responsible
for wrapping it around with HTTPS and WSS. on ports 443 and 8081,
respectively.

# Rules

* You win the game by getting all your seven pieces from one end of
  the board to the other.
* You roll marked four dice, and the sum of all dice with marks up is
  how much you can move a single piece on your turn.
* If your piece lands on a rosette, you gain an extra turn.
* You can jump over or capture enemy pieces, but you cannot capture
  pieces that are your own, or are on a rosette.


## License

BSD 3-Clause

