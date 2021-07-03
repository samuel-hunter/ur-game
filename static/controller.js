/* global model, view, WebSocket
 *
 * Controller - Handle callbacks from the UI and manages the game server
 * connection
 */

let controller = {};

(function () {

  let webSocket

  const socketCodeOpponentDisconnected = 4000
  const socketCodeSessionFull = 4002


  // Send a message to the game server
  function send(operand, data) {
    let message = Object.assign({op: operand}, data)

    // Heartbeat messages are annoying; don't print them out
    if (operand !== 'heartbeat') {
      console.log('sending', message)
    }

    webSocket.send(JSON.stringify(message))
  }

  // Return a function that sends an operand-only message.
  function simpleMessageCommand(operand) {
    return function sendSimpleMessage() {
      send(operand)
    }
  }

  controller.movePiece = function movePiece(position) {
    send('move', {position: position})
  }

  controller.sendComment = function sendComment() {
    let commentElem = document.getElementById('comment')
    let comment = commentElem.value.trim()

    // Comment only when meaningful text is included
    if (comment !== '')
      send('message', {message: comment})

    commentElem.value = ''
  }

  // Roll the dice. Called by #roll-button
  controller.roll = function roll() {
    /// XXX tight coupling between controller and view
    document.getElementById('roll-button').disabled = true
    send('roll')
  }

  controller.offerDraw = simpleMessageCommand('draw')
  controller.forfeit = simpleMessageCommand('forfeit')
  controller.rematch = simpleMessageCommand('rematch')

  controller.connect = function connect(token) {
    // Clear message box
    document.getElementById('messages').innerHTML = ''

    // XXX tight coupling between controller and view
    for (let b of document.getElementsByClassName('post-session')) {
      b.classList.add('hidden')
    }

    let socketUrl
    if (location.protocol === 'https:') {
      socketUrl = 'wss://'
    } else {
      socketUrl = 'ws://'
    }

    socketUrl += location.hostname +
		  (location.port ? (':' + location.port) : '') +
		  '/wss/sessions' +
		  (token ? ('/' + token) : '')

    webSocket = new WebSocket(socketUrl)

    // XXX the model should be ripped out and this entire callback ought to be
    // handled by the view.
    webSocket.onmessage = function (event) {
      let data = JSON.parse(event.data)

      // ACK messages are annoying; don't print them out
      if (data.op !== 'ack') {
        console.log('received', data)
      }

      // Handle operands which change the game's state.
      switch (data.op) {
      case 'gameStart':
        model.playerColor = data.color
        model.gameState = data.game
        break
      case 'gameState':
      case 'gameOver':
        model.gameState = data.game
        break
      case 'roll':
        if (data.successful) model.gameState.lastRoll = data.total
        break
      }

      model.updateStateArray()
      if (data.op === 'gameOver') model.setEndGame()

      // Defer the operand to the view.
      view.handleGameMessage(data)
    }

    webSocket.onopen = function (event) {
      window.setInterval(simpleMessageCommand('heartbeat'), 10_000)
    }

    // XXX this entire callback should be handled by the view.
    webSocket.onclose = function (event) {
      let message
      if (event.wasClean) {
        switch (event.code) {
          case socketCodeOpponentDisconnected:
            message = 'opponent disconnected'
            break
          case socketCodeSessionFull:
            message = 'session full'
          default:
            // Ideally unreachable code. Print out the code and reason anyways.
            console.warn(`Unhandled socket close code: ${event.code}`)

            message = `connection closed cleanly; code=${event.code}, reason="${event.reason}"`
            break
        }
      } else if (event.target === webSocket) {
        // Connection died on present websocket, and no new connection has been made.
        message = 'connection interrupted'
      }

      model.setDisconnected()
      view.endGame(message, false)
    }

    // XXX this entire callback should be handled by the view too.
    webSocket.onerror = function (error) {
      if (token) {
        // Try again, but this time creating a new game instead of
        // joining a preexisting game.
        console.warn('Retrying connection by making a new game')
        view.logActivity('game', "Couldn't join game. Asking for a new game")
        connect()
      } else {
        console.error(error)
        let message = 'socket error'
        if (error.message) message += ': ' + error.message

        model.setDisconnected()
        view.endGame(message, false)
      }
    }
  }

  function start() {
    let tokenRegex = /#\/(\w+)/
    let match = tokenRegex.exec(window.location.hash)
    if (match) {
      controller.connect(match[1])
    } else {
      controller.connect()
    }
  }

  if (document.readyState !== 'loading') {
    start()
  } else {
    document.addEventListener('DOMContentLoaded', start)
  }

})()
