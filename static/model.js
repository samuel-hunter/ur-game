var model = {
  playerColor: null,
  stateArray: ['disconnected', 'no-game'],
  gameState: {
    turn: null,
    lastRoll: null
  }
}; // Explicit semicolon so that JS doesn't mistake the parens as calling the object.

(function () {
  const rosettes = [4, 8, 14]

  model.tileType = function tileType(position) {
    if (rosettes.indexOf(position) >= 0) {
      return 'rosette'
    } else {
      return 'normal'
    }
  }

  model.colorToPlayer = function colorToPlayer(color) {
    return (model.playerColor === color) ? 'you' : 'opponent'
  }

  model.opponentColor = function opponentColor(color) {
    return (color === 'black') ? 'white' : 'black'
  }

  model.tileOwner = function tileOwner(color, position) {
    if (position <= 4) {
      return model.gameState[color].startPath[position - 1]
    } else if (position <= 12) {
      return model.gameState.sharedPath[position - 5]
    } else {
      return model.gameState[color].endPath[position - 13]
    }
  }

  // Return whether your piece can be moved to the destination.
  model.isValidDestination = function isValidDestination(position) {
    if (position < 1 || position > 15) return false
    if (position === 15) return true
    let tile = model.tileOwner(model.playerColor, position)
    if (tile === 'none') return true

    if (tile === model.playerColor) return false
    if (model.tileType(position) === 'rosette') return false
    return true
  }

  model.setEndGame = function setEndGame() {
    model.gameState.turn = null
    model.gameState.lastRoll = null
    model.stateArray = ['no-game', 'connected']
  }

  model.setDisconnected = function setDisconnected() {
    model.gameState.turn = null
    model.gameState.lastRoll = null
    model.stateArray = ['disconnected', 'no-game']
  }

  model.updateStateArray = function updateStateArray() {
    let stateArray = model.stateArray = ['connected']

    if (model.gameState.turn === null) {
      stateArray.push('no-game')
    } else {
      stateArray.push('active-game')
      stateArray.push(model.gameState.turn + '-turn')

      if (model.gameState.turn === model.playerColor) {
        stateArray.push('your-turn')
      } else {
        stateArray.push('opponent-turn')
      }

      if (model.gameState.lastRoll === null) {
        stateArray.push('roll-phase')
      } else {
        stateArray.push('move-phase')
      }
    }
  }

  model.hasState = function hasState(state) {
    return model.stateArray.indexOf(state) >= 0
  }

  model.hasAllStates = function hasAllStates(states) {
    for (let state of states) {
      if (!model.hasState(state)) return false
    }

    return true
  }
})()
