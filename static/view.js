/* global model, setTimeout, Notification, controller */

// Explicit semicolon so that JS doesn't mistake the parens as calling the object.
var view = {};

(function () {
  function getTileElement(color, position) {
    if (position > 4 && position < 13) {
      return document.getElementById('shared-' + position)
    } else {
      return document.getElementById(model.colorToPlayer(color) + '-' + position)
    }
  }

  function updateTooltip(message) {
    // Set the default message if a custom message isn't given.
    if (message === undefined) {
      if (model.gameState.turn === null) {
        message = 'Game over'
      } else if (model.gameState.turn !== model.playerColor) {
        message = "It is your opponent's turn"
      } else if (model.gameState.lastRoll === null) {
        message = "It is your turn"
      } else {
        message = `You rolled a ${model.gameState.lastRoll}`
      }
    }

    document.getElementById('tooltip').textContent = message
  }

  function clearBoard() {
    for (let tile of document.getElementsByClassName('tile')) {
      tile.innerHTML = ''
    }
  }

  function highlightDestination() {
    if (model.gameState.lastRoll === null) return

    let position = parseInt(this.getAttribute('data-position'))
    let destination = position + model.gameState.lastRoll

    var tile = getTileElement(model.playerColor, destination)

    if (tile) {
      tile.classList.add('selected')
    }

    if (!model.isValidDestination(destination)) {
      updateTooltip("You can't move there")
      this.classList.add('invalid-move')
    }
  }

  let pageTitle = document.title
  function unhighlightSelected() {
    updateTooltip()

    for (let elem of document.getElementsByClassName('selected')) {
      elem.classList.remove('selected')
    }

    for (let elem of document.getElementsByClassName('invalid-move')) {
      elem.classList.remove('invalid-move')
    }
  }

  // append a message to the client's message box
  function addTextMessage(source, message) {
    let messages = document.getElementById('messages')
    let messageElem = document.createElement('p')
    let sourceElem = document.createElement('strong')

    sourceElem.textContent = source
    messageElem.appendChild(sourceElem)
    messageElem.insertAdjacentText('beforeend', ' ' + message)

    messageElem.classList.add('recent')
    window.setTimeout(() => messageElem.classList.remove('recent'), 1000)

    messages.appendChild(messageElem)

    // Scroll down to see the latest comment
    messages.scrollTop = messages.scrollHeight

    // Alert the user with a change in the title.
    if (document.hidden) {
      document.title = '! ' + pageTitle
      setTimeout(function () { document.title = pageTitle }, 500)
    }
  }

  view.logActivity = function logActivity(source, message) {
    if (source === 'game') source += ':'
    source = source.charAt(0).toUpperCase() + source.slice(1)

    addTextMessage(source, message + '.')
  }

  function showComment(player, message) {
    player = player.charAt(0).toUpperCase() + player.slice(1)
    addTextMessage(player + ':', message)
  }

  function updateTurn() {
    updateTooltip()

    if (model.gameState.turn === model.playerColor) {
      view.logActivity('game', 'It is your turn')
    } else {
      view.logActivity('game', "It is your opponent's turn")
    }

    // Enable the roll button whenever it's your turn and you haven't rolled yet
    document.getElementById('roll-button').disabled = !(model.gameState.lastRoll === null &&
                                                        model.gameState.turn == model.playerColor)
  }

  function createPiece(color, position) {
    let piece = document.createElement('span')
    let player = model.colorToPlayer(color)

    if (player === 'you') {
      piece.onmouseenter = highlightDestination
      piece.onmouseleave = unhighlightSelected
      piece.onclick = function () {
        unhighlightSelected()

        let position = parseInt(this.getAttribute('data-position'))
        controller.movePiece(position)
      }
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

  // Repopulare the board with the appropriate pieces
  function updateBoard() {
    clearBoard()
    for (let color of ['black', 'white']) {
      // Populate player's starting path
      for (let i = 0; i < 4; i++) {
        if (model.gameState[color].startPath[i] === color)
          addPiece(color, i+1)
      }

      // Populate player's ending path
      for (let i = 0; i < 2; i++) {
        if (model.gameState[color].endPath[i] === color)
          addPiece(color, i+13)
      }

      // Populate the player's spare pieces
      let amnt = model.gameState[color].sparePieces
      let piecePool = document.getElementById(model.colorToPlayer(color) + '-pool')
      piecePool.innerHTML = ''
      for (var i = 0; i < amnt; i++) {
        let piece = createPiece(color, 0)
        piecePool.appendChild(piece)
      }
    }

    // Populare the game board's shared path
    for (let i = 0; i < 8; i++) {
      if (model.gameState.sharedPath[i] !== 'none')
        addPiece(model.gameState.sharedPath[i], i+5)
    }

  }

  function updateGameState() {
    for (let b of document.getElementsByClassName('post-game')) {
      b.classList.add('hidden')
    }

    updateTurn()
    updateBoard()

    // Enable game actions
    for (let gameButton of document.getElementsByClassName('game-button')) {
      gameButton.disabled = false
    }
  }

  // Clear and repopulate the dice pool with a list of dice results
  view.setDice = function setDice(points) {
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

  view.showInvite = function showInvite(url) {
    document.getElementById('invite-link').value = url
    document.getElementById('invite-modal').classList.remove('hidden')
  }

  view.hideInvite = function hideInvite() {
    document.getElementById('invite-modal').classList.add('hidden')
  }

  // Called by #invite-button
  view.copyInvite = function copyInvite() {
    let inviteLink = document.getElementById('invite-link')
    inviteLink.select()
    document.execCommand('copy')
  }

  view.gameOver = function gameOver(reason, sessionActive) {
    view.logActivity('game', 'Game over: ' + reason)

    let hide = (e) => e.classList.add('hidden')
    let show = (e) => e.classList.remove('hidden')

    let postGameOptions = document.getElementsByClassName('post-game')
    let disconnectedOptions = document.getElementsByClassName('post-session')

    if (sessionActive) {
      for (let b of postGameOptions) show(b)
      for (let b of disconnectedOptions) hide(b)
    } else {
      for (let b of postGameOptions) hide(b)
      for (let b of disconnectedOptions) show(b)
    }

    for (let gameButton of document.getElementsByClassName('game-button')) {
      gameButton.disabled = true
    }
    document.getElementById('roll-button').disabled = true

    model.gameState.turn = null
    model.gameState.lastRoll = null
    updateTooltip()
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

  function rollDice(total, flips, skipTurn, reason) {
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
  }

  view.handleGameMessage = function(data) {
    switch (data.op) {
    case 'gameToken':
      view.showInvite(window.location.protocol + '//' +
                      window.location.host + '/join/' + data.token)
      break
    case 'welcome':
      view.hideInvite()
      model.playerColor = data.color
      view.logActivity('game', 'Welcome. You are playing ' + model.playerColor)
      break
    case 'gameState':
      updateGameState()
      break
    case 'ack':
      break
    case 'roll':
      if (data.successful) {
        rollDice(data.total, data.flips, data.skipTurn, data.reason)

        // TODO: Figure out how to cleanly migrate this to controller.js
        if (data.skipTurn) {
          updateTurn()
          model.gameState.turn = model.opponentColor(model.gameState.turn)
        }
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
      showComment(model.colorToPlayer(data.color), data.message)
      break
    case 'gameOver':
      if (data.winner === null) {
        view.gameOver("it's a tie", true)
      } else if (data.winner === model.playerColor) {
        view.gameOver('you win', true)
      } else {
        view.gameOver('opponent wins', true)
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
})()
