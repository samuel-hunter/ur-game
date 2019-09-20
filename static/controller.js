/* global model, view, WebSocket */

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

function movePiece(position) {
  if (model.gameState.lastRoll === null) return
  if (model.gameState.turn !== model.playerColor) return

  if (!model.isValidDestination(position + model.gameState.lastRoll)) return

  sendGameMessage({op: 'move', position: position})
}

function sendComment() {
  let commentElem = document.getElementById('comment')
  let comment = commentElem.value.trim()

  // Comment only when meaningful text is included
  if (comment !== '') {
    sendGameMessage({op: 'message', message: comment})
  }

  commentElem.value = ''
}

// Roll the dice. Called by #roll-button
function roll() {
  document.getElementById('roll-button').disabled = true
  sendGameMessage({op: 'roll'})
}

function offerDraw() {
  sendGameMessage({op: 'draw'})
}

function forfeit() {
  sendGameMessage({op: 'forfeit'})
}

// Roll, offer draw, or forfeit when its respective keys are
// pressed
document.body.addEventListener('keypress', function (event) {
  // Block when typing in chat.
  if (document.activeElement.tagName === 'INPUT') return

  // Block when game is not in action
  if (model.gameState.turn === null) return

  // Block when the roll button is disabled.
  if (event.key === 'r' &&
      !document.getElementById('roll-button').disabled) roll()

  if (event.key === 'd') offerDraw()
  if (event.key === 'f') forfeit()
})

function showInvite(token) {
  let inviteLink = document.getElementById('invite-link')
  inviteLink.value = window.location.protocol + '//' +
    window.location.host + '/join/' + token

  document.getElementById('invite-modal').classList.remove('hidden')
}

function hideInvite() {
  document.getElementById('invite-modal').classList.add('hidden')
}

// Called by #invite-button
function copyInvite() {
  let inviteLink = document.getElementById('invite-link')
  inviteLink.select()
  document.execCommand('copy')
}

function gameOver(reason) {
  view.logActivity('game', 'Game over: ' + reason)

  let postGameOptions = document.getElementById('post-game-options')
  let disconnectedOptions = document.getElementById('disconnected-options')

  if (webSocket.readyState === WebSocket.OPEN) {
    postGameOptions.classList.remove('hidden')
    disconnectedOptions.classList.add('hidden')
  } else {
    disconnectedOptions.classList.remove('hidden')
    postGameOptions.classList.add('hidden')
  }

  for (let gameButton of document.getElementsByClassName('game-button')) {
    gameButton.disabled = true
  }
  document.getElementById('roll-button').disabled = true

  model.gameState.turn = null
  model.gameState.lastRoll = null
  view.updateTooltip()
}

function rematch() {
  sendGameMessage({op: 'rematch'})
}

function rollDice(total, flips, skipTurn, reason) {
  if (model.gameState.turn === model.playerColor) {
    model.gameState.lastRoll = total
    view.setDice(flips)
  }

  let message = 'rolled a ' + total
  switch (skipTurn) {
  case 'flippedNothing':
    message += ' and skipped a turn'
    break
  case 'noValidMoves':
    message += ', and with no valid moves, skipped a turn'
    break
  }
  view.logActivity(model.colorToPlayer(model.gameState.turn), message)

  if (skipTurn)
    view.updateTurn(model.opponentColor(model.gameState.turn))
}

function getMoveMessage(moveType) {
  switch (moveType) {
  case 'movedPiece':
    return 'moved a piece'
    break
  case 'completedPiece':
    return "completed a piece's full trip"
    break
  case 'landedOnRosette':
    return 'landed on a rosette, netting an extra turn'
    break
  case 'capturedPiece':
    return 'captured an enemy piece'
    break
  default:
    // Something went wrong here, but it's not the end of the world.
    console.warn(`Unexpected moveType: ${moveType}`)

    return `performed an unexpected move (${moveType})`
  }
}

function connect(token) {
  // Clear message box
  document.getElementById('messages').innerHTML = ''

  // Hide relevant button group
  document.getElementById('disconnected-options').classList.add('hidden')

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

    switch (data.op) {
    case 'gameToken':
      showInvite(data.token)
      break
    case 'welcome':
      hideInvite()
      model.playerColor = data.color
      view.logActivity('game', 'Welcome. You are playing ' + model.playerColor)
      break
    case 'gameState':
      view.updateGameState(data.game, movePiece)
      break
    case 'ack':
      break
    case 'roll':
      if (data.successful) {
        rollDice(data.total, data.flips, data.skipTurn, data.reason)
      } else {
        view.logActivity('game', "Can't roll: " + data.reason)
      }
      break
    case 'move':
      if (data.successful) {
        view.logActivity(model.colorToPlayer(model.gameState.turn), getMoveMessage(data.moveType))
      } else {
        view.logActivity('game', "Can't move: " + data.reason)
      }
      break
    case 'message':
      view.showComment(model.colorToPlayer(data.color), data.message)
      break
    case 'gameOver':
      if (data.winner === null) {
        gameOver("it's a tie")
      } else if (data.winner === model.playerColor) {
        gameOver('you win')
      } else {
        gameOver('opponent wins')
      }
      break
    case 'tie':
      view.logActivity(data.player, 'offers a tie')
      break
    case 'forfeit':
      view.logActivity(data.player, 'forfeits')
      break
    case 'err':
      view.logActivity('game', "Error: " + data.reason)
      break
    default:
      view.logActivity('game', 'Unhandled op ' + data.op)
      break
    }
  }

  webSocket.onopen = function (event) {
    window.setInterval(() => sendGameMessage({op: 'heartbeat'}), 10000)
  }

  webSocket.onclose = function (event) {

    if (event.wasClean) {
      switch (event.code) {
      case socketCodeOpponentDisconnected:
        gameOver('opponent disconnected')
        break
      default:
        gameOver(`connection closed cleanly; code=${event.code}, reason="${event.reason}"`)
        break
      }
    } else if (event.target === webSocket) {
      // Connection died on present websocket, and no new connection has been made.
      gameOver('connection died')
    }

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
      if (error.message) {
        gameOver('socket error: ' + error.message)
      } else {
        gameOver('socket error')
      }
    }

  }
}

function start() {
  let socketUrl = 'ws://' + document.domain + ':8081'
  let tokenRegex = /#\/(\w+)/

  let match = tokenRegex.exec(window.location.hash)
  if (match) {
    connect(match[1])
  } else {
    connect()
  }


  document.getElementById('comment').addEventListener('keyup', function (event) {
  if (event.key === 'Enter') {
    sendComment()
  }
  })

  view.setDice([0, 0, 0, 0])
}

if (document.readyState != 'loading') {
  start()
} else {
  document.addEventListener('DOMContentLoaded', start)
}
