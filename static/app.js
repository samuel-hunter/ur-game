/* global WebSocket */

var playerColor = 'white'
var lastRoll = 1

function updateRoll() {
  lastRoll = parseInt(document.getElementById('roll').value)
}

function toPlayer (color) {
  if (playerColor == 'white' ^ color == 'white') {
    return 'opponent'
  } else {
    return 'you'
  }
}

function getTile (color, position) {
  if (position > 4 && position < 13) {
    return document.getElementById('shared-' + position)
  } else {
    return document.getElementById(toPlayer(color) + '-' + position)
  }
}

function removePiece (tile) {
  tile.innerHTML = ''
}

function clearBoard () {
  for (let tile of document.getElementsByClassName('tile')) {
    removePiece(tile)
  }
}

function isValidDestination(tile) {
  if (tile.children.length === 0) return true

  let piece = tile.children[0]
  if (piece.getAttribute('data-player') === 'you') return false
  if (tile.classList.contains('rosette')) return false
  return true
}

function highlightDestination () {
  let position = parseInt(this.getAttribute('data-position'))
  var tile = getTile(playerColor, position + lastRoll)
  tile.classList.add('selected')

  if (!isValidDestination(tile)) {
    this.classList.add('invalid-move')
  }
}

function unhighlightSelected () {
  for (let elem of document.getElementsByClassName('tile')) {
    elem.classList.remove('selected')
  }

  for (let elem of document.getElementsByClassName('invalid-move')) {
    elem.classList.remove('invalid-move')
  }
}


function createPiece (color, position) {
  var piece = document.createElement('span')
  let player = toPlayer(color)

  if (player === 'you') {
    piece.onmouseenter = highlightDestination
    piece.onmouseleave = unhighlightSelected
  }

  piece.setAttribute('data-position', position)
  piece.setAttribute('data-player', player)
  piece.classList.add('piece', color)
  return piece
}

function addPiece (color, position) {
  var tile = getTile(color, position)
  var piece = createPiece(color, position)
  tile.appendChild(piece)
}

function setSparePieces (color, amnt) {
  let player = toPlayer(color)
  var piecePool = document.getElementById(player + '-pool')
  piecePool.innerHTML = ''
  for (var i = 0; i < amnt; i++) {
    var piece = createPiece(color, 0)
    piecePool.appendChild(piece)
  }
}

function logActivity(player, message) {
  var history = document.getElementById('history-box')
  var comment = document.createElement('p')
  var source = document.createElement('strong')
  player = player.charAt(0).toUpperCase() + player.slice(1)

  source.insertAdjacentText('beforeend', player)
  comment.appendChild(source)
  comment.insertAdjacentText('beforeend', ' ' + message + '.')
  history.appendChild(comment)

  // Scroll down to see the latest comment
  history.scrollTop = history.scrollHeight
}

function start () {
  var socketUrl = 'ws://' + document.domain + ':8081/new'
  var webSocket = new WebSocket(socketUrl)

  webSocket.onmessage = function (event) {
    var data = JSON.parse(event.data)
    console.log({message: 'received', data: data})
  }

  webSocket.onopen = function (event) {
    var data = {op: 'heartbeat'}
    console.log({message: 'sending', data: data})
    webSocket.send(JSON.stringify(data))
  }

  addPiece('white', 2)
  addPiece('white', 8)
  addPiece('black', 1)
  setSparePieces('white', 5)
  setSparePieces('black', 6)

  logActivity('you', 'rolled a 3')
  logActivity('you', 'captured an enemy piece')
  logActivity('oppoent', 'rolled a 4')
  logActivity('opponent', 'landed on a rosette, netting another turn')
  logActivity('you', 'rolled a 0, skipping your turn')
  logActivity('opponent', 'rolled a 3, providing no valid moves and skipping their turn')
  updateRoll()
}

if (document.readyState != 'loading') {
  start()
} else {
  document.addEventListener('DOMContentLoaded', start)
}
