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

## Endpoint

The endpoint to create a new session is `wss://$host/sessions/new`. Endpoints
to join preexisting sessions are provided by the Session Join event.

## Requests

Requests are POJ (Plain Old JSON). They always have a `request` field that
contains a URI, and may have additional fields depending on the request:

```json
{ "request": "$uri" }
```

The URI's will be provided by the server, and it's recommended that the client
uses those instead of hardcoded URI's.

### Heartbeat

A heartbeat tells the server it's alive. It's recommended for now to make this
request about every 5 seconds. The server should automatically disconnect a
client that doesn't respond after about 30 seconds:

```json
{ "request": "$uri" }
```

### Game State

Request for the game state.

```json
{ "request": "$uri" }
```

### Roll

Request for the client's payer to roll the dice:

```json
{ "request": "$uri" }
```

### Move

Request for the client's player to move from a specific position. Postion 0 is
from the spare pile, and positions 1 onward is from the board itself:

```json
{
	"request": "$uri",
	"position": integer
}
```

### Draw

Offer a draw to the opponent player:

```json
{ "request": "$uri" }
```

### Rematch

Request rematch for the opponent player:

```json
{ "request": "$uri" }
```

### Forfeit

Request the client's player forefiets the game:

```json
{ "request": "$uri" }
```

### Message

Request to send a message:

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
		"message": "$uri",
		"game": "$uri"
	},
	// The Game resource is usually embedded if the event changes the game state.
	"_embedded": {
		"game": {
			"_links": {
				"self": {"href": "$uri"},
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

- `heartbeat`
- `message`
- `rematch`

### The Game Resource and Hypermedia Controls

The Game resource is structured like this, along with a list of all possible
links (usually, some are hidden, as they would be invalid depending on the
state of the game).

```json
{
	"_links": {
		"self": {"href": "$uri"},
		"roll": {"href": "$uri"},
		"move": {"href": "$uri"},
		"draw": {"href": "$uri"},
		"rematch": {"href": "$uri"},
		"forfeit": {"href": "$uri"}
	},
	"black": {
		"startPath": [ "$space", "$space", "$space", "$space" ],
		"endPath": [ "$space", "$space" ],
		"sparePieces": integer
	},
	"white": {
		"startPath": [ "$space", "$space", "$space", "$space" ],
		"endPath": [ "$space", "$space" ],
		"sparePieces": integer
	},
	"sharedPath": [ "$space", "$space", "$space", "$space", "$space", "$space", "$space", "$space" ],
	"lastRoll": roll,
	"turn": "$player"
}
```

(A `$player` value can be either `white` or `black`, and a `space` value can
be a player or `none`. A `roll` may either be `null` or an array of `1` or
`0` values.)

### Ack

Acknowledges a heartbeat message:

```json
{
	"event": "/ack",
	"_links": { ... }
```

### Session Join

Sent on connection when the client join a session. If the client is the only
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

Indicates the turn-controlling player moved. `moveType` describes the type of move made. `endedTurn` indicates whether the move ends the current turn. Embeds the Game resource:

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
