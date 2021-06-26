# Networking Plan

## Procedure Outline

1. Alice creates a web socket connection to the server. It sends a
   game creation request.
2. The web server generates a game session token and sends it to
   Alice, who can then send it to her friend, Bob, to join the game.
3. Meanwile, Alice sends a heartbeat every so often to demonstrate
   that she is alive. The server sends an acknowledgement back. If the
   server doesn't receive a heartbeat after a lengthy period (say,
   15sec) then the server closes connection and closes the
   game. Similarly, if Alice doesn't receive an ACK within a given
   time, she closes the connection herself.
4. Bob creates a web socket connection to the server. It sends a game
   join request with the token he received from Alice.
5. By this time, Bob also sends heartbeat messages like in Step 3.
6. The game server randomly decides from the two connections who is
   White and who is Black. The tokens for each player is
   generated. The server gives each individual player their color and
   token to identify themselves. In this case, Alice is White and Bob
   is Black.
7. As per rules of the game, White goes first. The server broadcasts a
   'snapshot' of the game state to both players to start the turn.
   Alice sends a message (with her token) to roll the dice. The server
   broadcasts the result to both players. Alice then tells the server
   to move a piece from Tile #0 (to indicate from her remaining piece
   pool) and finishes her turn. The turn ends.
8. The next turn starts, and the game sends another snapshot of the
   state. Bob rolls the dice. He decides to move from Tile 1, but the
   Server responds as an invalid move. Bob moves from Tile 0, and ends
   his turn.
9. It's Alice's turn and, like before, rolls the dice. Bob, using an
   altered client, decides to move 'for' Alice. However, without
   Alice's token, Bob gets an authentication error.
10. After multiple moves, Alice's finishing turn clears her last
    piece. The server sends a 'Game Over' message with the victor as
    White, which Alice interprets as her win and Bob his loss. The
    server disconnects both players' sockets.

## Technical Outline

1. Alice to Server: NEWGAME
2. Server to Alice: OK #:GAMETOK
3. Alice to Server: HEARTBEAT
   Server to Alice: ACK
4. Bob to Server: JOINGAME #:GAMETOK
5. Bob to Server: HEARTBEAT
   Server to Bob: ACK
6. Server to Alice: PLAYER WHTIE #:WHTIETOK
   Server to Bob: PLAYER BLACK #:BLACKTOK
7. Server to All: GAMESTATE ( .... )
   Alice to Server: ROLL #:WHITETOK
   Server to Alice: ROLLOK 2 (1 0 0 1) NIL
   (note: the NIL turns into a descriptive symbol explaiing if and why
          the server skips the players' turn.
   Alice to Server: MOVE 0 #:WHITETOK
   Server to Alice: MOVEOK NORMAL-MOVE
8. Bob to Server: ROLL #:BLACKTOK
   Server to Bob: ROLLOK 2 (0 1 0 0) NIL
   Bob to Server: MOVE 1 #:BLACKTOK
   Server to Bob: MOVEBAD UNOWNED-TILE
   Bob to Server: MOVE 0 #:BLACKTOK
   Server to Bob: MOVEOK NORMAL-MOVE
9. Alice to Server: ROLL #:WHITETOK
   Server to Alice: ROLLOK 3 (1 1 0 1) NIL
   Bob to Server: MOVE 2 #:SOMETOK
   Server to Bob: MOVEBAD AUTH-ERR
   Alice to Server: MOVE 0 #:WHITETOK
   Server to Alice: MOVEOK NORMAL-MOVE
10. ...
    Alice to Server: MOVE 14 #:WHITETOK
    Server to ALl: GAMEOVER WHTIE
