/* global model, view, WebSocket */

let controller = {};

(function () {

  let webSocket

  const socketCodeOpponentDisconnected = 4000


  // Send a message to the game server
  function sendGameMessage(data) {
    // Heartbeat messages are annoying; don't print them out
    if (data.op !== 'heartbeat') {
      console.log({message: 'sending', data: data})
    }
    webSocket.send(JSON.stringify(data))
  }

  controller.movePiece = function movePiece(position) {
    if (model.gameState.lastRoll === null) return
    if (model.gameState.turn !== model.playerColor) return

    if (!model.isValidDestination(position + model.gameState.lastRoll)) return

    sendGameMessage({op: 'move', position: position})
  }

  controller.sendComment = function sendComment() {
    let commentElem = document.getElementById('comment')
    let comment = commentElem.value.trim()

    // Comment only when meaningful text is included
    if (comment !== '') {
      sendGameMessage({op: 'message', message: comment})
    }

    commentElem.value = ''
  }

  // Roll the dice. Called by #roll-button
  controller.roll = function roll() {
    document.getElementById('roll-button').disabled = true
    sendGameMessage({op: 'roll'})
  }

  controller.offerDraw = function offerDraw() {
    sendGameMessage({op: 'draw'})
  }

  controller.forfeit = function forfeit() {
    sendGameMessage({op: 'forfeit'})
  }

  controller.rematch = function rematch() {
    sendGameMessage({op: 'rematch'})
  }

  controller.connect = function connect(token) {
    // Clear message box
    document.getElementById('messages').innerHTML = ''

    // Hide relevant buttons
    for (let b of document.getElementsByClassName('post-session')) {
      b.classList.add('hidden')
    }

    let socketUrl
    if (window.location.protocol === 'https:') {
      socketUrl = 'wss://'
    } else {
      socketUrl = 'ws://'
    }

    socketUrl += document.domain + ':8081'
    if (token) {
      socketUrl += '/join/' + token
    } else {
      socketUrl += '/new'
    }

    webSocket = new WebSocket(socketUrl)
    webSocket.onmessage = function (event) {
      let data = JSON.parse(event.data)

      // ACK messages are annoying; don't print them out
      if (data.op !== 'ack') {
        console.log({message: 'received', data: data})
      }

      // Handle operands which change the game's state.
      switch (data.op) {
      case 'welcome':
        model.playerColor = data.color
        break
      case 'gameState':
        model.gameState = data.game
        break
      case 'roll':
        if (data.successful) model.gameState.lastRoll = data.total
        break
      }

      // Defer the operand to the view.
      view.handleGameMessage(data)
    }

    webSocket.onopen = function (event) {
      window.setInterval(() => sendGameMessage({op: 'heartbeat'}), 10000)
    }

    webSocket.onclose = function (event) {
      let message
      if (event.wasClean) {
        switch (event.code) {
        case socketCodeOpponentDisconnected:
          message = 'opponent disconnected'
          break
        default:
          // Ideally unreachable code. Print out the code and reason anyways.
          console.warn(`Unhandled socket close code: ${event.code}`)

          message = `connection closed cleanly; code=${event.code}, reason="${event.reason}"`
          break
        }
      } else if (event.target === webSocket) {
        // Connection died on present websocket, and no new connection has been made.
        message = 'connection died'
      }

      view.endGame(message, false)
    }

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
        view.endGame(message, false)
      }
    }
  }

  function start() {
    let socketUrl = 'ws://' + document.domain + ':8081'
    let tokenRegex = /#\/(\w+)/

    let match = tokenRegex.exec(window.location.hash)
    if (match) {
      controller.connect(match[1])
    } else {
      controller.connect()
    }


    document.getElementById('comment').addEventListener('keyup', function (event) {
      if (event.key === 'Enter') {
        controller.sendComment()
      }
    })

    // Roll, offer draw, or forfeit when its respective keys are
    // pressed
    document.body.addEventListener('keypress', function (event) {
      // Block when typing in chat.
      if (document.activeElement.tagName === 'INPUT') return

      // Block when game is not in action
      if (model.gameState.turn === null) return

      // Block when the roll button is disabled.
      if (event.key === 'r' &&
          !document.getElementById('roll-button').disabled) controller.roll()

      if (event.key === 'd') controller.offerDraw()
      if (event.key === 'f') controller.forfeit()
    })
  }

  if (document.readyState != 'loading') {
    start()
  } else {
    document.addEventListener('DOMContentLoaded', start)
  }

})()
