/* global WebSocket */

const socketCodeOpponentDisconnected = 4000
const rosettes = [4, 8, 14]

let webSocket
let playerColor
let gameState = {
  turn: null,
  lastRoll: null
}

// Return the type of tile
function tileType(position) {
  if (rosettes.indexOf(position) >= 0) {
    return 'rosette'
  } else {
    return 'normal'
  }
}

function colorToPlayer(color) {
  return (playerColor === color) ? 'you' : 'opponent'
}

function opponentColor(color) {
  return (color === 'black') ? 'white' : 'black'
}

function getTileOwner(color, position) {
  if (position <= 4) {
    return gameState[color].startPath[position - 1]
  } else if (position <= 12) {
    return gameState.sharedPath[position - 5]
  } else {
    return gameState[color].endPath[position - 13]
  }
}

function getTileElement(color, position) {
  if (position > 4 && position < 13) {
    return document.getElementById('shared-' + position)
  } else {
    return document.getElementById(colorToPlayer(color) + '-' + position)
  }
}

// Return whether your piece can be moved to the destination.
function isValidDestination(position) {
  if (position < 1 || position > 15) return false
  if (position === 15) return true
  let tile = getTileOwner(playerColor, position)
  if (tile === 'none') return true

  if (tile === playerColor) return false
  if (tileType(position) === 'rosette') return false
  return true
}

function updateTooltip(message) {
  // Set the default message if a custom message isn't given.
  if (message === undefined) {
    if (gameState.turn === null) {
      message = 'Game over'
    } else if (gameState.turn !== playerColor) {
      message = "It is your opponent's turn"
    } else if (gameState.lastRoll === null) {
      message = "It is your turn"
    } else {
      message = `You rolled a ${gameState.lastRoll}`
    }
  }

  let tooltip = document.getElementById('tooltip')
  tooltip.innerHTML = ''
  tooltip.insertAdjacentText('beforeend', message)
}

function clearBoard() {
  for (let tile of document.getElementsByClassName('tile')) {
    tile.innerHTML = ''
  }
}

function highlightDestination() {
  if (gameState.lastRoll === null) return

  let position = parseInt(this.getAttribute('data-position'))
  let destination = position + gameState.lastRoll

  var tile = getTileElement(playerColor, destination)

  if (tile) {
    tile.classList.add('selected')
  }

  if (!isValidDestination(destination)) {
    updateTooltip("You can't move there")
    this.classList.add('invalid-move')
  }
}

function unhighlightSelected() {
  updateTooltip()

  for (let elem of document.getElementsByClassName('selected')) {
    elem.classList.remove('selected')
  }

  for (let elem of document.getElementsByClassName('invalid-move')) {
    elem.classList.remove('invalid-move')
  }
}


function createPiece(color, position) {
  let piece = document.createElement('span')
  let player = colorToPlayer(color)

  if (player === 'you') {
    piece.onmouseenter = highlightDestination
    piece.onmouseleave = unhighlightSelected
    piece.onclick = movePiece
  }

  piece.setAttribute('data-position', position)
  piece.setAttribute('data-player', player)
  piece.classList.add('piece', color)
  return piece
}

// Add a piece to the board
function addPiece(color, position) {
  getTileElement(color, position).appendChild(createPiece(color, position))
}

// Display the spare pieces on the appropriate side of the game board.
function setSparePieces(color, amnt) {
  let player = colorToPlayer(color)
  let piecePool = document.getElementById(player + '-pool')
  piecePool.innerHTML = ''
  for (var i = 0; i < amnt; i++) {
    let piece = createPiece(color, 0)
    piecePool.appendChild(piece)
  }
}

// append a message to the client's message box
function addTextMessage(source, message) {
  let messages = document.getElementById('messages')
  let messageElem = document.createElement('p')
  let sourceElem = document.createElement('strong')

  sourceElem.insertAdjacentText('beforeend', source)
  messageElem.appendChild(sourceElem)
  messageElem.insertAdjacentText('beforeend', ' ' + message)

  messageElem.classList.add('recent')
  window.setTimeout(() => messageElem.classList.remove('recent'), 1000)

  messages.appendChild(messageElem)

  // Scroll down to see the latest comment
  messages.scrollTop = messages.scrollHeight
}

function logActivity(source, message) {
  if (source === 'game') source += ':'
  source = source.charAt(0).toUpperCase() + source.slice(1)

  addTextMessage(source, message + '.')
}

function showComment(player, message) {
  player = player.charAt(0).toUpperCase() + player.slice(1)
  addTextMessage(player + ':', message)
}

function updateTurn(newTurn) {
  if (newTurn !== undefined) {
    gameState.turn = newTurn
  }
  updateTooltip()

  if (gameState.turn === playerColor) {
    logActivity('game', 'It is your turn')
  } else {
    logActivity('game', "It is your opponent's turn")
  }

  // Enable the roll button whenever it's your turn and you haven't rolled yet
  document.getElementById('roll-button').disabled = !(gameState.lastRoll === null &&
                                                      gameState.turn == playerColor)
}

// Repopulare the board with the appropriate pieces
function updateBoard() {
  clearBoard()
  for (let color of ['black', 'white']) {
    // Populate player's starting path
    for (let i = 0; i < 4; i++) {
      if (gameState[color].startPath[i] === color)
        addPiece(color, i+1)
    }

    // Populate player's ending path
    for (let i = 0; i < 2; i++) {
      if (gameState[color].endPath[i] === color)
        addPiece(color, i+13)
    }

    // Populate the player's spare pieces
    setSparePieces(color, gameState[color].sparePieces)
  }

  // Populare the game board's shared path
  for (let i = 0; i < 8; i++) {
    if (gameState.sharedPath[i] !== 'none')
      addPiece(gameState.sharedPath[i], i+5)
  }

}

function updateGameState(newGameState) {
  gameState = newGameState
  document.getElementById('post-game-options').classList.add('hidden')

  updateTurn()
  updateBoard()

  // Enable game actions
  for (let gameButton of document.getElementsByClassName('game-button')) {
    gameButton.disabled = false
  }
}

// Send a message to the game server
function sendGameMessage(data) {
  // Heartbeat messages are annoying; don't print them out
  if (data.op !== 'heartbeat') {
    console.log({message: 'sending', data: data})
  }
  webSocket.send(JSON.stringify(data))
}

function movePiece() {
  if (gameState.lastRoll === null) return
  if (gameState.turn !== playerColor) return

  let position = parseInt(this.getAttribute('data-position'))
  if (!isValidDestination(position + gameState.lastRoll)) return

  sendGameMessage({op: 'move', position: position})
  unhighlightSelected()
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

// Clear and repopulate the dice pool with a list of dice results
function setDice(points) {
  let dicePool = document.getElementById('dice-pool')
  dicePool.innerHTML = ''

  let dieTemplate = document.getElementById('die-template')

  for (let i = 0; i < 4; i++) {
    let die = dieTemplate.cloneNode(true)
    die.classList.add('die')
    die.classList.remove('hidden')
    die.id = null

    let pips = die.getElementsByClassName('pip')

    var sidePips
    if (points[i] === 1) {
      sidePips = 1
    } else {
      die.getElementsByClassName('center-pip')[0].classList.add('hidden')
      sidePips = 2
    }

    // Hide side pips until there's two total pips left
    for (let pip = 0; pip < 3; pip++) {
      if (Math.random() > sidePips / (3 - pip)) {
        pips[pip].classList.add('hidden')
      } else {
        sidePips--
      }
    }

    dicePool.appendChild(die)
  }
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
  if (gameState.turn === null) return

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
  logActivity('game', 'Game over: ' + reason)

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

  gameState.turn = null
  gameState.lastRoll = null
  updateTooltip()
}

function rematch() {
  sendGameMessage({op: 'rematch'})
}

function rollDice(total, flips, skipTurn, reason) {
  if (gameState.turn === playerColor) {
    gameState.lastRoll = total
    setDice(flips)
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
  logActivity(colorToPlayer(gameState.turn), message)

  if (skipTurn)
    updateTurn(opponentColor(gameState.turn))
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
      playerColor = data.color
      logActivity('game', 'Welcome. You are playing ' + playerColor)
      break
    case 'gameState':
      updateGameState(data.game)
      break
    case 'ack':
      break
    case 'roll':
      if (data.successful) {
        rollDice(data.total, data.flips, data.skipTurn, data.reason)
      } else {
        logActivity('game', "Can't roll: " + data.reason)
      }
      break
    case 'move':
      if (data.successful) {
        logActivity(colorToPlayer(gameState.turn), getMoveMessage(data.moveType))
      } else {
        logActivity('game', "Can't move: " + data.reason)
      }
      break
    case 'message':
      showComment(colorToPlayer(data.color), data.message)
      break
    case 'gameOver':
      if (data.winner === null) {
        gameOver("it's a tie")
      } else if (data.winner === playerColor) {
        gameOver('you win')
      } else {
        gameOver('opponent wins')
      }
      break
    case 'tie':
      logActivity(data.player, 'offers a tie')
      break
    case 'forfeit':
      logActivity(data.player, 'forfeits')
      break
    case 'err':
      logActivity('game', "Error: " + data.reason)
      break
    default:
      logActivity('game', 'Unhandled op ' + data.op)
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
      logActivity('game', "Couldn't join game. Asking for a new game")
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

  setDice([0, 0, 0, 0])
}

if (document.readyState != 'loading') {
  start()
} else {
  document.addEventListener('DOMContentLoaded', start)
}
