# API as It's Planned to Be

The main goal for this API is to provide HATEOAS for two-way client-server communication.
There will be two distinct types of messages: requests (from client to server) and events (from server to client).
Events may embed resources, and resources may provide an ID, a list of hypermedia controls, and a list of event callbacks.

I'll be using a homegrown protocol ontop of JSON on WebSockets because I don't see any good standard for the time being, just articles explaining that REST is not enough.
I'll call it HERL for now (Hypertext Events and Requests Language) because it sounds similar enough to HAL to call it good.

## What I'm Looking Out of HERL

I want HERL to be structured enough that I can abstract away the event callbacks and requests on the client-side, and make it easy to provide resource controls and events on the server-side, but still flexible enough that it only cares about those two main parts.
I want HERL to provide both of these through hypermedia controls, so the only *large* magic string the client needs to know about is the connection endpoint.
All other ways to connect through are from small-word string-value "relations" between resources.

I think roughly, I want the browser side to look like this:

```js
HERL.connect('/ws/sessions/new')
	.on('@', (session) => {
		session.on('game-start', (data) => {
			view.setupGame(data.game)

			data.game.on(...)
		})
		.on('chat', (data) => {
			view.sendMessage(data.from, data.message)
		})
	})
	.onError((data) => {
		view.sendMessage('Game', 'Error: ' + data.summary)
	})
	.sendRequest('ready')
```

And I think going for a command pattern makes sense for the backend:

```lisp
;; TODO
```

I also have some game-specific goals I'd use to shape HERL:
This is going to be for a game, so it makes little sense to "subscribe" to individual events to listen in.
The client connected to a session, so they're going to hear all events related to that session.

## HERL

There are four key Things: Requests, Events, Resources, and an Error Resource.
Requests are sent from the client to "request" the server do some action, and they always have an `@request` field that holds a URI string.
Events are sent from the server to inform clients something happened, and they always have an `@event` field that holds a URI string.
Resources and Events have none or more of Request hypermedia controls, Event hypermedia controls, and other embedded Resources.
Resources store Request hyperlinks in the `@requests` field, Event hyperlinks in the `@events` field, and embedded resources in the `@embedded` field.
All three of these fields (when provided) are JSON objects that map a relation to the Resource to the Thing in question.

An example event with an embedded resource may look like this:

```js
{
	"@event": "/game/move",
	"@events": {
		"chat": { "href": "/chat" }
	},
	"@embedded": {
		"game": { ... }
	},
	"moveType": "capture"
}
```

An example request may look like:

```js
{
	"@request": "move",
	"position": [ 2, 3 ]
}
```

*TODO explain hypermedia anatomy*

## Endpoint

The endpoint to create a new session is `wss://$host/ws/sessions`.
Endpoints to join preexisting sessions are in the form `wss://$host/ws/sessions/:token`.

## Game Resource

*TODO*

```json
{
	"@events": {
		"roll": { "href": "/game/roll" },
		"move": { "href": "/game/move" },
		"draw": { "href": "/game/draw" },
		"game-over": { "href": "/game/game-over" }
	},
	"@requests": {
		"roll": { "href": "/game/roll" },
		"move": { "href": "/game/move" },
		"draw": { "href": "/game/draw" },
		"forfeit": { "href": "/game/forfeit" }
	},
	"boardPieces": [ [ "$space", ... ], ... ]
	"blackPool": integer,
	"whitePool": integer,
	"lastRoll": roll,
	"turn": "$player"
}
```

A `$player` may either be `black` or `white`.
A `roll` may either be an array of `1` or `0` values, or `null`.
The `boardPieces` field is an array of rows, of which rows are arrays of spaces.

## Session Resource

??? *TODO*

## Session-bound Events

Session-bound events always provide `chat` and `rematch` requests when available, and always provide the `newGame` event.

### New Game

Indicates a new game has been made.
Embeds the Game resource and Board:

```json
{
	"@event": "/new-game",
	"@events": { ... },
	"@requests": { ... },
	"@embedded": { "@game": ... },
	"boardColumns": 10,
	"boardRows": 3,
	"board": [ [ "$tile", ... ], ... ]
}
```

A `tile` may either be `normal`, `empty`, or `rosette`.

### Token

Indicates a session has been born, and needs a new player.
Provides a `token` field to join the session at the endpoint `/ws/sessions/:token`:

```json
{
	"@event": "/token",
	"@events": { ... },
	"@requests": { ... },
	"token": "$token"
}
```

### Chat

Indicates someone has said something.
Provides a `from` field (either `"you"` or `"them"`) and `message` string field:

```json
{
	"@event": "chat",
	"@events": { ... },
	"@requests": { ... },
	"from": "$color",
	"message": "$message"
}
```

## Session-bound Requests

*TODO*

### Chat

Request to send a chat message:

```json
{
	"@request": "$uri",
	"message": "message"
}
```

### Rematch

Request to rematch the opponent after a game has finished.

```json
{ "@request": "$uri" }
```

## Game-bound Events

*TODO*

### Roll

*TODO*

```json
{ "@request": "$uri" }
```

### Move

*TODO*

```json
{
	"@request": "$uri",
	"position": position
}
```

### Draw

*TODO*

```json
{ "@request": "$uri" }
```

## Game-bound Requests

*TODO*

### Roll

Request for the client's player to roll the dice.

```json
{ "@request": "$uri" }
```

### Move

Request for the client player's checker to move from a specific grid position.
The position is either `"pool"`, to indicate it comes from the player's pool, or an array of two integers to indicate a zero-based grid position:

```json
{
	"@request": "$uri",
	"position": gridPosition
}
```

### Draw

Offer a draw to the opponent player:

```json
{ "@request": "$uri" }
```

### Forfeit

Request to forfeit the match.

```json
{ "@request": "$uri" }
```
