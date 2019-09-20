/* global model */

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

  view.updateTooltip = function updateTooltip(message) {
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
      view.updateTooltip("You can't move there")
      this.classList.add('invalid-move')
    }
  }

  function unhighlightSelected() {
    view.updateTooltip()

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
  }

  view.logActivity = function logActivity(source, message) {
    if (source === 'game') source += ':'
    source = source.charAt(0).toUpperCase() + source.slice(1)

    addTextMessage(source, message + '.')
  }

  view.showComment = function showComment(player, message) {
    player = player.charAt(0).toUpperCase() + player.slice(1)
    addTextMessage(player + ':', message)
  }

  view.updateTurn = function updateTurn(newTurn) {
    if (newTurn !== undefined) {
      model.gameState.turn = newTurn
    }
    view.updateTooltip()

    if (model.gameState.turn === model.playerColor) {
      view.logActivity('game', 'It is your turn')
    } else {
      view.logActivity('game', "It is your opponent's turn")
    }

    // Enable the roll button whenever it's your turn and you haven't rolled yet
    document.getElementById('roll-button').disabled = !(model.gameState.lastRoll === null &&
                                                        model.gameState.turn == model.playerColor)
  }

  function createPiece(color, position, onclick) {
    let piece = document.createElement('span')
    let player = model.colorToPlayer(color)

    if (player === 'you') {
      piece.onmouseenter = highlightDestination
      piece.onmouseleave = unhighlightSelected
      piece.onclick = function () {
        unhighlightSelected()

        if (onclick) {
          let position = parseInt(this.getAttribute('data-position'))
          onclick(position)
        }
      }
    }

    piece.setAttribute('data-position', position)
    piece.setAttribute('data-player', player)
    piece.classList.add('piece', color)
    return piece
  }

  // Add a piece to the board
  function addPiece(color, position, onclick) {
    getTileElement(color, position).appendChild(createPiece(color, position, onclick))
  }

  // Repopulare the board with the appropriate pieces
  function updateBoard(onPieceClick) {
    clearBoard()
    for (let color of ['black', 'white']) {
      // Populate player's starting path
      for (let i = 0; i < 4; i++) {
        if (model.gameState[color].startPath[i] === color)
          addPiece(color, i+1, onPieceClick)
      }

      // Populate player's ending path
      for (let i = 0; i < 2; i++) {
        if (model.gameState[color].endPath[i] === color)
          addPiece(color, i+13, onPieceClick)
      }

      // Populate the player's spare pieces
      let amnt = model.gameState[color].sparePieces
      let piecePool = document.getElementById(model.colorToPlayer(color) + '-pool')
      piecePool.innerHTML = ''
      for (var i = 0; i < amnt; i++) {
        let piece = createPiece(color, 0, onPieceClick)
        piecePool.appendChild(piece)
      }
    }

    // Populare the game board's shared path
    for (let i = 0; i < 8; i++) {
      if (model.gameState.sharedPath[i] !== 'none')
        addPiece(model.gameState.sharedPath[i], i+5, onPieceClick)
    }

  }

  view.updateGameState = function updateGameState(newGameState, onclick) {
    model.gameState = newGameState
    document.getElementById('post-game-options').classList.add('hidden')

    view.updateTurn()
    updateBoard(onclick)

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

})()
