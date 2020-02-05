# ur-game

This will be a web server that hosts 2-player sessions of [The Royal
Game of Ur](https://en.wikipedia.org/wiki/Royal_Game_of_Ur), inspired
by this [YouTube video](https://www.youtube.com/watch?v=WZskjLq040I)
from The British Museum:

[![Tom Scott vs Irving Finkel: The Royal Game of
Ur](https://img.youtube.com/vi/WZskjLq040I/0.jpg)](https://youtu.be/WZskjLq040I
"Tom Scott vs Irving Finkel: The Royal Game of Ur")

# Host the Software

The service is deployed using docker. To run without HTTPS:

    $ docker build -t ur-game .
    $ docker run -d -p 8080:8080 -p 8081:8082 ur-game

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
