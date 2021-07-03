# The JavaScript Plan

The javascript is a bit of a mess right now. This document gives me space to
think about how to reorganize the MVC I have at the moment.

## Overall Design

### Model

Should be completely removed JS-wise. The Model should be manipulated by the
Controller, and should update the View. If we think of the server as the Model,
then messages outbound to the server map well as manipulation, and events
inbound from the server map well to updates. Assigning the server the full role
of Model makes best sense here.

### View

The View's job is to process messages from the Model (server) and update the
UI. Its external functions are laid out for the controller *solely* for the
purpose of calling with cleaned-up server events. The View does the brunt of
message-processing, error-processing, and so on. The View's main side-effect is
manipulating the DOM to show the current game state, and to enable/disable
controls that send messages to the Controller.

### Controller

The Controller's job is to expose functions the UI that sends messages to the
game server.

## View Events

These events are specifically for the API as it is right now.

### Connection end
- Append event to message box
- Clear the board
- Update the tooltip
- Send a message to the messagebox
- Show New Session button
- Disable all in-session buttons
- Enable all out-session buttons (only New Session for now)

### Connection start
- Enable all in-session, out-game buttons except for Rematch (only Chat for now)
- Disable all out-session buttons (only New Session for now)
- Do nothing else, server always follows-up with an initial operand

### Connection error
- Append event to message box
- Do everything like Connection end
- Probably don't try to restart connection

### ack
- Deprecated, should use WS ping frames instead (handled entirely by browser)

### message
- Append to message box.

### err
- ???
- What error messages do we send again?
- Probably update the message box or tooltip

### gameToken
- Pop up the invite model with an invite link

### gameStart
- Hide the invite modal
- Remember what your player color is
- Do everything gameState does

### gameState
- Update the game board and tooltip
- Send a message to the messagebox
- Enable or disable input buttons depending on whose turn it is
- $.currentRole is deprecated, it's always roll phase.

### tie
- Append event to message box

### forfeit
- Append event to message box

### roll
- Update dice visualization
- If it's client's turn, disable roll button and enable piece clicking.

### move
- Append event to message box

### gameOver
- Append event to message box
- Clear game board
- Disable in-game buttons
- Enable out-game buttons (just rematch for now)

### rematch
- Deprecated

## Controller functions

The Controller exposes a function for each request, and that should be about
it. It's the View's main responsibility for providing the information favorable
for the controller. The Controller doesn't want to know what a DOM is.
