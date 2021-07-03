# API as it's planned to be

The goal is for communication to appriach Level 3 Maturity on the Richardson REST
Maturity model. Most of the API will be within a websocket with two types of
endpoints: one endpoint to create a new session, and one to join a preexisting
session.

In reality, the API won't be stateless. I think it makes sense at a minimum for
the server to remember which player the client is, which breaks the basic
statelessness principle of REST from what I understand. That said, I still see
benefit into adhering to the other principles while keeping the statefullness
that *is* there to a minimum.

## Communication model

There are two types of messages: clients send requests, servers send events.
All of them are in JSON (application/json), but server-side messages are
additionally in JSON HAL (application/hal+json).

As part of reaching HAL, server events will try to provide hypermedia controls
within their messages to promote the self-discovery that RESTful glory
advertises.

Finally, beyond requests and events, the server will also send occasional
WebSocket ping frames.

## Endpoint

The endpoint to create a new session is `wss://$host/ws/sessions`. Endpoints to
join preexisting sessions are in the form `wss://$host/ws/sessions/:token`.

## Requests

Requests are POJ (Plain Old JSON). They always have a `request` field that
contains a URI, and may have additional fields depending on the request:

```json
{ "request": "$uri" }
```

The URI's will be provided by the server, and it's strongly recommended that
the client uses those instead of hardcoded URI's.

### Game Board

Request for the board resource. Hypermedia control reveals itself on in-game
events as the `board` relation, or within `board` resources as the `self`
relation:

```json
{ "request": "$uri" }
```

### Game State

Request for the current state of the game. Hypermedia control reveals itself on
in-game events as the `game` relation, or within `game` resources as the `self`
relation:

```json
{ "request": "$uri" }
```

### Roll

Request for the client's payer to roll the dice. Hypermedia control reveals
itself on game resources as the `roll` relation:

```json
{ "request": "$uri" }
```

### Move

Request for the client player's piece to move from a specific grid position.
The position is either `"pool"`, to indicate to move from the player pool, or
an array of two integers to indicate a zero-based grid position. Hypermedia
control reveals itself on game resources as the `move` relation:

```json
{
	"request": "$uri",
	"position": gridPosition
}
```

### Draw

Offer a draw to the opponent player. Hypermedia control reveals itself on game
resources as the `draw` relation:

```json
{ "request": "$uri" }
```

### Rematch

Request rematch for the opponent player. Hypermedia control reveals itself on
game resources as the `rematch` relation:

```json
{ "request": "$uri" }
```

### Forfeit

Request the client's player forfeits the game. Hypermedia control reveals
itself on game resources as the `forfeit` relation:

```json
{ "request": "$uri" }
```

### Chat

Request to send a chat message. Hypermedia control reveals itself on in-session
events as the `chat` relation:

```json
{
	"request": "$uri",
	"message": "$message"
}
```

## Events

Events are HAL JSON and always have an `event` URI field. Events usually have
other POJ fields depending on the event, and some events may embed resources
(usually the Game state) related to the event. Wherever possible, events and
their embedded resources provide links to requests the client is permitted to
make.

An example event, populated with links and an embedded Game resource, may look
like:

```json
{
	"event": "$uri",
	// some event data...
	"_links": {
		"chat": "$uri",
		"game": "$uri"
	},
	// The Game resource is usually embedded if the event changes the game state.
	"_embedded": {
		"game": {
			"_links": {
				"self": {"href": "$uri"},
				"board": {"href": "$uri"},
				"roll": {"href": "$uri"},
				"draw": {"href": "$uri"},
				"forfeit": {"href": "$uri"}
			},
			// some game data...
		}
	}
}
```

**All** events provide Session-related hypermedia controls in `$._links`. Any
of the links may be hidden in an individual control depending on whether the
client is forbidden from making that request. The fields are:

- `chat`
- `rematch`

### The Game Resource and Hypermedia Controls

The Game resource is structured like this, along with a list of all possible
links (usually, some are hidden, as they would be invalid depending on the
state of the game).

```json
{
	"_links": {
		"self": {"href": "$uri"},
		"board": {"href": "$uri"},
		"roll": {"href": "$uri"},
		"move": {"href": "$uri"},
		"draw": {"href": "$uri"},
		"rematch": {"href": "$uri"},
		"forfeit": {"href": "$uri"}
	},
	"boardPieces": [ [ "$space", ... ], ... ]
	"blackPool": integer,
	"whitePool": integer,
	"lastRoll": roll,
	"turn": "$player"
}
```

(A `$player` value can be either `white` or `black`, and a `space` value can
be a player or `none`. A `roll` may either be `null` or an array of `1` or
`0` values.)

### The Board Resource and Hypermedia Controls

The Board resource holds `nrows` and `ncolumns` integer fields, and a 2D array of tiles:

```json
{
	"_links": {
		"self": {"href": "$uri"}
	},
	"nrows": integer,
	"ncolumns": integer,
	"rows": [ [ "$tile", ... ], ... ]
}
```

A `$tile` for the time being can be either `empty`, `normal`, or `rosette`.

### Session Join

Sent on connection when the client joins a session. If the client is the only
one in the session, provides a URL for another client to join:

```json
{
	"event": "/join",
	"joinUrl": "$url" or null,
	"_links": { ... }
}
```

### Game Start

Marks the beginning of a game, usually when two clients connect or a rematch
has initiated. Embeds the Game resource:

```json
{
	"event": "/games/@this/start",
	"_links": {
		...,
		"game": { "href": "/games/@this" }
	},
	"_embedded": {
		"game": { ... }
	}
}
```

### Game Over

Indicates the game is over, providing the winner and the final game state.
Embeds the Game resource:

```json
{
	"event": "/games/@this/over",
	"winner": "$player",
	"reason": "$reason",
	"_links": {
		...,
		"game": { "href": "$uri" }
	},
	"_embedded": {
		"game": { ... }
	}
}
```

### Message

Indicates a player has sent a message:

```json
{
	"event": "/message",
	"player": "$player",
	"message": string,
	"_links": { ... }
}
```

### Error

Indicates an internal server error. May result in a premature termination. If
the session is in-game, embeds the Game resource:

```json
{
	"event": "/error",
	"code": "$code",
	"description": "$description",
	"_links": {
		...,
		"game": { "href": "$game" }
	},
	"_embedded": {
		"game": { ... }
	}
}
```

### Denied

Indicates a client request has been denied, either because it is malformed or
doesn't make sense with the session/game's current state. If the session is
in-game, embeds the Game resource:

```json
{
	"event": "/denied",
	"fromUri": "$uri",
	"reason": "$reason",
	"_links": {
		...,
		"game": { "href": "$uri" }
	},
	"_embedded": {
		"game": { ... }
	}
}
```

### Tie

Indicates a player wishes to tie a game, but the game has not yet tied.

```json
{
	"event": "/games/@this/tie",
	"player": "$player",
	"_links": { ... }
}
```

### Roll

Indicates the turn-controlling player rolled, and whether it results in a
skipped turn. Embeds the Game resource:

```json
{
	"event": "/games/@this/roll",
	"skippedTurn": boolean,
	"_links": {
		...,
		"game": { "href": "$uri" }
	},
	"_embedded": {
		"game": { ... }
	}
}
```

### Move

Indicates the turn-controlling player moved. `moveType` describes the type of
move made. `endedTurn` indicates whether the move ends the current turn. Embeds
the Game resource:

```
{
	"event": "/games/@this/move",
	"moveType": moveType,
	"skippedTurn": boolean,
	"_links": {
		...,
		"game": { "href": "$uri" }
	},
	"_embedded": {
		"game": { ... }
	}
}
```
