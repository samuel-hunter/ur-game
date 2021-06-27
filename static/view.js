/* global model, setTimeout, Notification, controller */

// Explicit semicolon so that JS doesn't mistake the parens as calling the object.
var view = {};

(function () {
  let keybinds = {}

  function updateTooltip(message) {
    // Set the default message if a custom message isn't given.
    if (message === undefined) {
      if (model.hasState('no-game')) {
        message = 'Game over'
      } else if (model.hasState('opponent-turn')) {
        message = "It is your opponent's turn"
      } else if (model.hasState('roll-phase')) {
        message = "It is your turn"
      } else {
        message = `You rolled a ${model.gameState.lastRoll}`
      }
    }

    document.getElementById('tooltip').textContent = message
  }

  function getTileElement(color, position) {
    if (position > 4 && position < 13) {
      return document.getElementById('shared-' + position)
    } else {
      return document.getElementById(model.colorToPlayer(color) + '-' + position)
    }
  }

  function clearBoard() {
    for (let tile of document.getElementsByClassName('tile')) {
      tile.innerHTML = ''
    }
  }

  let pageTitle = document.title
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
    if (model.hasState('your-turn')) {
      view.logActivity('game', 'It is your turn')
    } else {
      view.logActivity('game', "It is your opponent's turn")
    }
  }

  function highlightDestination() {
    if (!model.hasAllStates(['your-turn', 'move-phase'])) return

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

  view.endGame = function endGame(reason, sessionActive) {
    view.logActivity('game', 'Game over: ' + reason)
    updateTooltip()
    updateGameButtons()
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

  function showDice(total, flips, skipTurn, reason) {
    setDice(flips)

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

  function updateGameButtons() {
    for (let button of document.getElementsByClassName('game-button')) {
      let type = button.getAttribute('data-button')
      let state = button.getAttribute('data-state').split(' ')
      let statesFulfilled = model.hasAllStates(state)

      switch (type) {
      case 'true':
        button.disabled = !statesFulfilled
        break
      case 'hidden':
        if (statesFulfilled) {
          button.classList.remove('hidden')
        } else {
          button.classList.add('hidden')
        }
        break
      default:
        console.error(`Unknown game button type '${type}'`)
        break
      }
    }
  }

  view.handleGameMessage = function(data) {
    switch (data.op) {
    case 'gameToken':
      view.showInvite(window.location.protocol + '//' +
                      window.location.host + '/join/' + data.token)
      break
    case 'gameStart':
      view.hideInvite()
      view.logActivity('game', 'Welcome. You are playing ' + model.playerColor)

      updateTurn()
      updateBoard()
      break
    case 'gameState':
      updateTurn()
      updateBoard()
      break
    case 'ack':
      break
    case 'roll':
      if (data.successful) {
        showDice(data.total, data.flips, data.skipTurn, data.reason)

        // TODO: Figure out how to cleanly migrate this to controller.js
        if (data.skipTurn) {
          model.gameState.turn = model.opponentColor(model.gameState.turn)
          model.gameState.lastRoll = null
          model.updateStateArray()
          updateTurn()
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
        view.endGame("it's a tie", true)
      } else if (data.winner === model.playerColor) {
        view.endGame('you win', true)
      } else {
        view.endGame('opponent wins', true)
      }

      updateBoard()
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

    updateGameButtons()
    updateTooltip()
  }

  document.getElementById('comment').addEventListener('keyup', function (event) {
    if (event.key === 'Enter') {
      controller.sendComment()
    }
  })

  function start() {
    setDice([0, 0, 0, 0])

    for (let button of document.getElementsByClassName('game-button')) {
      let key = button.getAttribute('data-keybind')
      let state = button.getAttribute('data-state').split(' ')
      if (key === null) continue

      keybinds[key] = {
        state: state,
        func: new Function(button.getAttribute('onclick'))
      }
    }

    document.body.addEventListener('keypress', function (event) {
      // Block when typing in chat
      if (document.activeElement.tagName === 'INPUT') return

      if (event.key in keybinds) {
        let keybind = keybinds[event.key]

        if (model.hasAllStates(keybind.state)) keybind.func()
      }
    })
  }

  if (document.readyState != 'loading') {
    start()
  } else {
    document.addEventListener('DOMContentLoaded', start)
  }
})()
