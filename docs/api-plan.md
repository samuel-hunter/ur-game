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

### Session Requests

All session-specific requests are provided as links for every event.

#### Rematch

Request rematch for the opponent player. Hypermedia control reveals itself on
game resources as the `rematch` relation:

```json
{ "request": "$uri" }
```

#### Chat

Request to send a chat message. Hypermedia control reveals itself on in-session
events as the `chat` relation:

```json
{
	"request": "$uri",
	"message": "$message"
}
```

### Game Requests

All game-specific requests are provided as links within every game resource.

#### Roll

Request for the client's payer to roll the dice. Hypermedia control reveals
itself on game resources as the `roll` relation:

```json
{ "request": "$uri" }
```

#### Move

Request for the client player's checker to move from a specific grid position.
The position is either `"pool"`, to indicate to move from the player pool, or
an array of two integers to indicate a zero-based grid position. Hypermedia
control reveals itself on game resources as the `move` relation:

```json
{
	"request": "$uri",
	"position": gridPosition
}
```

#### Draw

Offer a draw to the opponent player. Hypermedia control reveals itself on game
resources as the `draw` relation:

```json
{ "request": "$uri" }
```

#### Forfeit

Request the client's player forfeits the game. Hypermedia control reveals
itself on game resources as the `forfeit` relation:

```json
{ "request": "$uri" }
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
	// some event data...
	"_links": {
		"chat": "$uri"
	},
	// The Game resource is usually embedded if the event changes the game state.
	"_embedded": {
		"game": {
			"_links": {
				"roll": {"href": "$uri"},
				"draw": {"href": "$uri"},
				"forfeit": {"href": "$uri"}
			},
			// some game data...
		}
	},
	"event": "$uri"
}
```

### The Game Resource and Hypermedia Controls

The Game resource is structured like this, along with a list of all possible
links (usually, some are hidden, as they would be invalid depending on the
state of the game).

```json
{
	"_id": "@this",
	"_links": {
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

TODO: read over this

A `$player` may either be `black` or `white`. A `roll` may either be

The `boardPieces` are an array of rows, of which rows are arrays. A `$space` may either be `white`, `black`, or `empty` for

(A `$player` value can be either `white` or `black`, and a `space` value can
be a player or `none`. A `roll` may either be `null` or an array of `1` or
`0` values.)

### The Board Resource and Hypermedia Controls

The Board resource holds `nrows` and `ncolumns` integer fields, and a 2D array of tiles:

```json
{
	"nrows": integer,
	"ncolumns": integer,
	"rows": [ [ "$tile", ... ], ... ]
}
```

A `$tile` for the time being can be either `empty`, `normal`, or `rosette`.

### Session Create

Sent on connection when the client creates a new session. Provides a URI for
another client to join:

```json
{
	"_links": {
		"chat": {"href": "$uri"}
	}
	"event": "/join",
	"joinUrl": "$url" or null,
}
```

### Game Start

Marks the beginning of a game, when two clients connect or a rematch has
initiated. The second client who joins a session receives this on connection.
Embeds the Game and Board resources:

```json
{
	"_links": {
		"chat": {"href": "$uri"},
		...
	},
	"_embedded": {
		"game": { ... },
		"board": { ... }
	},
	"event": "/games/@this/start"
}
```

### Game Over

Indicates the game is over, providing the winner and the final game state.
Embeds the Game resource:

```json
{
	"_links": {
		...,
		"game": { "href": "$uri" }
	},
	"_embedded": {
		"game": { ... }
	},
	"event": "/games/@this/over",
	"winner": "$player",
	"reason": "$reason"
}
```

### Message

Indicates a player has sent a message:

```json
{
	"_links": { ... },
	"event": "/message",
	"player": "$player",
	"message": string
}
```

### Error

Indicates an internal server error. May result in a premature termination. If
the session is in-game, embeds the Game resource:

```json
{
	"_links": {
		...,
		"game": { "href": "$game" }
	},
	"_embedded": {
		"game": { ... }
	},
	"event": "/error",
	"code": "$code",
	"description": "$description"
}
```

### Denied

Indicates a client request has been denied, either because it is malformed or
doesn't make sense with the session/game's current state. Sends an overall
message, and a list of errors that boiled into the given denial.

```json
{
	"_links": { ... },
	"event": "/denied",
	"fromUri": "$uri",
	"message": "$message",
	"errors": [ "$error", ... ]
}
```

### Tie

Indicates a player wishes to tie a game, but the game has not yet tied.

```json
{
	"_links": { ... },
	"event": "/games/@this/tie",
	"player": "$player"
}
```

### Roll

Indicates the turn-controlling player rolled, and whether it results in a
skipped turn. Embeds the Game resource:

```json
{
	"_links": {
		...,
		"game": { "href": "$uri" }
	},
	"_embedded": {
		"game": { ... }
	},
	"event": "/games/@this/roll",
	"skippedTurn": boolean
}
```

### Move

Indicates the turn-controlling player moved. `moveType` describes the type of
move made. `endedTurn` indicates whether the move ends the current turn. Embeds
the Game resource:

```
{
	"_links": {
		...,
		"game": { "href": "$uri" }
	},
	"_embedded": {
		"game": { ... }
	},
	"event": "/games/@this/move",
	"moveType": moveType,
	"skippedTurn": boolean
}
```
