/* global WebSocket */

let playerColor = 'white'
let turn = 'white'
let lastRoll = null
let webSocket

function toPlayer(color) {
  if (playerColor == 'white' ^ color == 'white') {
    return 'opponent'
  } else {
    return 'you'
  }
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
  if (message === undefined) {
    if (turn !== playerColor) {
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

  // Enable the roll button whenever it's your turn
  document.getElementById('roll-button').disabled = (turn !== playerColor)
}

function updateGameState(game) {
  clearBoard()

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

  setTurn(game.turn)
}

function logActivity(player, message) {
  let history = document.getElementById('history-box')
  let comment = document.createElement('p')
  let source = document.createElement('strong')

  if (player === 'game') player += ':'
  player = player.charAt(0).toUpperCase() + player.slice(1)

  // Set as recent for a second
  comment.classList.add('recent')
  window.setTimeout(() => comment.classList.remove('recent'), 1000)

  source.insertAdjacentText('beforeend', player)
  comment.appendChild(source)
  comment.insertAdjacentText('beforeend', ' ' + message + '.')
  history.appendChild(comment)

  // Scroll down to see the latest comment
  history.scrollTop = history.scrollHeight
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

function sendMessage(data) {
  console.log({message: 'sending', data: data})
  webSocket.send(JSON.stringify(data))
}

function showInvite(token) {
  let inviteLink = document.getElementById('invite-link')
  inviteLink.value = window.location.host + '/#/' + token

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

function connect(token) {
  let socketUrl = 'ws://' + document.domain + ':8081'
  if (token) {
    socketUrl += '/join/' + token
  } else {
    socketUrl += '/new'
  }

  webSocket = new WebSocket(socketUrl)
  webSocket.onmessage = function (event) {
    let data = JSON.parse(event.data)
    console.log({message: 'received', data: data})

    switch (data.op) {
    case 'gameToken':
      showInvite(data.token)
      break
    case 'welcome':
      hideInvite()
      playerColor = data.color
      break
    case 'gameState':
      updateGameState(data.game)
      break
    case 'ack':
      break
    case 'roll':
      if (data.successful) {
        if (turn === playerColor) {
          lastRoll = data.total
          setDice(data.flips)
        }

        let message = 'rolled a ' + data.total
        switch (data.skipTurn) {
        case 'flippedNothing':
          message += ' and skipped a turn'
          break
        case 'noValidMove':
          message += ', and with no valid moves, skipped a turn'
          break
        }
        logActivity(toPlayer(turn), message)

        if (data.skipTurn) {
          if (turn === 'white') {
            setTurn('black')
          } else {
            setTurn('white')
          }
        }
      } else {
        logActivity('game', "Can't roll: " + data.reason)
      }
      break
    case 'move':
      if (data.successful) {
        let message = 'moved a piece'

        switch (data.moveType) {
        case 'movedPiece':
          message = 'moved a piece'
          break
        case 'completedPiece':
          message = "completed a piece's full trip"
          break
        case 'landedOnRosette':
          message = 'landed on a rosette, netting an extra turn'
          break
        case 'capturedPiece':
          message = 'captured an enemy piece'
          break
        }

        logActivity(toPlayer(turn), message)
      } else {
        logActivity('game', "Can't move: " + data.reason)
      }
      lastRoll = null
      break
    default:
      logActivity('game', 'Unhandled op ' + data.op)
    }
  }

  webSocket.onopen = function (event) {
    window.setInterval(() => sendMessage({op: 'heartbeat'}), 5000)
  }

  webSocket.onclose = function (event) {
    if (event.wasClean) {
      logActivity('game', `connection closed cleanly; code=${event.code}, reason=${event.reason}`)
    } else if (event.target === webSocket) {
      // Connection died on present websocket, and no new connection has been made.
      logActivity('game', 'connection died')
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
      let message = error.message
      if (message) {
        logActivity('game', 'Socket error: ' + error.message)
      } else {
        logActivity('game', 'Socket error')
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

  setDice([0, 0, 0, 0])
}

if (document.readyState != 'loading') {
  start()
} else {
  document.addEventListener('DOMContentLoaded', start)
}
