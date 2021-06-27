# API as it stands right now

## Communication model

All data is sent between client and server through JSON objects. Each JSON
object has an `op` field, which describes the core action, and all other fields
are data that describe the specifics of that action.

E.g. from server to client:

```json
{
	"op": "gameStart",
	"color": "white",
	"game": { ... }
}
```

(This describes that a game has started, that the player's color is white, and
gives the starting state of the game).

Within this documentation, an `integer` are any non-negative number, a `player`
can be either `"white"` or `"black"`, and a `space` can be a player or
`"none"`.

## Game Resource

Multiple server-bound messages embed the state of the game within the `game`
field. It includes the pieces on the game board, the player's turn, and so on.
A `game` within this documentation is this Game resource.

Object structure:

```json
{
	"black": {
		"startPath": [ space, space, space, space ],
		"endPath": [ space, space ],
		"sparePieces": integer
	},
	"white": {
		"startPath": [ space, space, space, space ],
		"endPath": [ space, space ],
		"sparePieces": integer
	},
	"sharedPath": [ space, space, space, space, space, space, space, space ],
	"lastRoll": null,
	"turn": player
}
```

(A `player` value can be either `"white"` or `"black"`, and a `space` value can
be a player or `"none"`.)

## Server Operations

### gameState

Sends over the game state.

```json
{
	"op": "gameState",
	"game": game
}
```

### gameStart

Indicates the start of the game, with the starting board and starting player
marked as `color`:

```json
{
	"op": "gameStart",
	"color": player,
	"game": game
}
```

NOTE: `$.color` is exactly the same as `$.game.turn`, making it redundant.

### gameToken

Sends a game session token to a client to give to a player and start a game:

```json
{
	"op": "gameToken",
	"token": string
}
```

The URL to contruct to make an invite will be `"$host/#/$token"`.

### gameOver

Indicates the game is over, indicating the winner and the final game state.

```json
{
	"op": "gameOver",
	"winner": player,
	"game": game
}
```

### ack

Acknowledges a heartbeat message:

```json
{ "op": "ack" }
```

### message

Indicates a message was sent from either player:

```json
{
	"op": "message",
	"color": player,
	"message": string
}
```

### err

Indicates a client request error, analogous to HTTP 4xx error codes:

```json
{
	"op": "err",
	"reason": reason
}
```

The `reason` can either be `"notPlayingYet"`, `"notGameOver"`, `"notYourTurn"`,
or `"noSuchOperand"`.

### tie

Indicates that a player wishes to tie the game. If all players indicate a wish
to tie, then the game is over and a `gameOver` message is sent.

```json
{
	"op": "tie",
	"player": player
}
```

### forfeit

Indicates that a player forfeits the game:

```json
{
	"op": "forfeit",
	"player": player
}
```

### roll

Indicates the turn-controlling player rolled:

```json
{
	"op": "roll",
	"successful": boolean,
	"total": integer,
	"points": [ oneOrZero, oneOrZero, oneOrZero, oneOrZero ],
	"skipTurn": boolean
}
```

### move

Indicates the turn-controlling player moved:

```json
{
	"op": "move",
	"successful": boolean,
	"moveType": "movedPiece",
}
```

## Client Operations

### heartbeat

Indicates the client is still alive.

```json
{ "op": "heartbeat" }
```

### message

Indicates the client player requests to send a message.

```json
{
	"op": "message",
	"message": string
}
```

### rematch

Indicates the client player requests to start a rematch.

```json
{ "op": "rematch" }
```

### draw

Indicates the client player requests to draw the game.

```json
{ "op": "draw" }
```

### forfeit

Indicates the client player requests to forfeit the game.

```json
{ "op": "forfeit" }
```

### roll

Indicates the client player wishes to roll the dice.

```json
{ "op": "roll" }
```

### move

Indicates the client player wishes to move one of their pieces.

```json
{
	"op": "move",
	"position": integer
}
```
