/* global WebSocket */

let playerColor = 'white'
let turn = null
let lastRoll = null
let webSocket

const socketCodeOpponentDisconnected = 4000

// Convert `white' or `black' to `you' or `opponent'.
function toPlayer(color) {
  return (playerColor === color) ? 'you' : 'opponent'
}

function getTile(color, position) {
  if (position > 4 && position < 13) {
    return document.getElementById('shared-' + position)
  } else {
    return document.getElementById(toPlayer(color) + '-' + position)
  }
}

function isValidDestination(position) {
  if (position < 1 || position > 15) return false
  if (position === 15) return true
  let tile = getTile(playerColor, position)
  if (tile.children.length === 0) return true

  let piece = tile.children[0]
  if (piece.getAttribute('data-player') === 'you') return false
  if (tile.classList.contains('rosette')) return false
  return true
}

function updateTooltip(message) {
  // Set the default message if a custom message isn't given.
  if (message === undefined) {
    if (turn === null) {
      message = 'Game over'
    } else if (turn !== playerColor) {
      message = "It is your opponent's turn"
    } else if (lastRoll === null) {
      message = "It is your turn"
    } else {
      message = `You rolled a ${lastRoll}`
    }
  }

  let tooltip = document.getElementById('tooltip')
  tooltip.innerHTML = ''
  tooltip.insertAdjacentText('beforeend', message)
}

function removePiece(tile) {
  tile.innerHTML = ''
}

function clearBoard() {
  for (let tile of document.getElementsByClassName('tile')) {
    removePiece(tile)
  }
}

function highlightDestination() {
  if (lastRoll === null) return

  let position = parseInt(this.getAttribute('data-position'))
  let destination = position + lastRoll

  var tile = getTile(playerColor, destination)

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
  let player = toPlayer(color)

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

function addPiece(color, position) {
  let tile = getTile(color, position)
  let piece = createPiece(color, position)
  tile.appendChild(piece)
}

function setSparePieces(color, amnt) {
  let player = toPlayer(color)
  let piecePool = document.getElementById(player + '-pool')
  piecePool.innerHTML = ''
  for (var i = 0; i < amnt; i++) {
    let piece = createPiece(color, 0)
    piecePool.appendChild(piece)
  }
}

function movePiece() {
  if (lastRoll === null) return
  if (turn !== playerColor) return

  let position = parseInt(this.getAttribute('data-position'))
  if (!isValidDestination(position + lastRoll)) return

  sendMessage({op: 'move', position: position})
  unhighlightSelected()
}

function setTurn(color) {
  turn = color
  updateTooltip()

  if (turn === playerColor) {
    logActivity('game', "It is your turn")
  } else {
    logActivity('game', "It is your opponent's turn")
  }

  // Enable the roll button whenever it's your turn and you haven't rolled yet
  document.getElementById('roll-button').disabled = lastRoll || (turn !== playerColor)
}

function updateGameState(game) {
  document.getElementById('post-game-options').classList.add('hidden')
  console.log({action: 'updateGameState', game: game})
  clearBoard()

  lastRoll = game.lastRoll

  for (let i = 0; i < 4; i++) {
    if (game.blackStart[i] === 'black')
      addPiece('black', i+1)

    if (game.whiteStart[i] === 'white')
      addPiece('white', i+1)
  }

  for (let i = 0; i < 8; i++) {
    if (game.sharedPath[i] !== 'none')
      addPiece(game.sharedPath[i], i+5)
  }

  for (let i = 0; i < 2; i++) {
    if (game.blackEnd[i] === 'black')
      addPiece('black', i+13)

    if (game.whiteEnd[i] === 'white')
      addPiece('white', i+13)
  }

  setSparePieces('white', game.whiteSparePieces)
  setSparePieces('black', game.blackSparePieces)

  for (let gameButton of document.getElementsByClassName('game-button')) {
    gameButton.disabled = false
  }

  setTurn(game.turn, game.lastRoll)
}

function addMessage(source, message) {
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

  addMessage(source, message + '.')
}

function showComment(player, message) {
  player = player.charAt(0).toUpperCase() + player.slice(1)
  addMessage(player + ':', message)
}

function sendComment() {
  let commentElem = document.getElementById('comment')
  let comment = commentElem.value.trim()

  // Comment only when meaningful text is included
  if (comment !== '') {
    sendMessage({op: 'message', message: comment})
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
  sendMessage({op: 'roll'})
}

function offerDraw() {
  sendMessage({op: 'draw'})
}

function forfeit() {
  sendMessage({op: 'forfeit'})
}

// Roll, offer draw, or forfeit when its respective keys are
// pressed
document.body.addEventListener('keypress', function (event) {
  // Block when typing in chat.
  if (document.activeElement.tagName === 'INPUT') return

  // Block when the roll button is disabled.
  if (event.key === 'r' &&
      !document.getElementById('roll-button').disabled) roll()

  if (event.key === 'd') offerDraw()
  if (event.key === 'f') forfeit()

})


function sendMessage(data) {
  // Heartbeat messages are annoying; don't print them out
  if (data.op !== 'heartbeat') {
    console.log({message: 'sending', data: data})
  }
  webSocket.send(JSON.stringify(data))
}

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
  turn = null
  lastRoll = null
  updateTooltip()
}

function rematch() {
  sendMessage({op: 'rematch'})
}

function rollDice(total, flips, skipTurn, reason) {
  if (turn === playerColor) {
    lastRoll = total
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
  logActivity(toPlayer(turn), message)

  if (skipTurn) {
    if (turn === 'white') {
      setTurn('black')
    } else {
      setTurn('white')
    }
  }
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
        logActivity(toPlayer(turn), getMoveMessage(data.moveType))
      } else {
        logActivity('game', "Can't move: " + data.reason)
      }
      break
    case 'message':
      showComment(toPlayer(data.color), data.message)
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
    window.setInterval(() => sendMessage({op: 'heartbeat'}), 10000)
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
