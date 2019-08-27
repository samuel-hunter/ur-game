/* global WebSocket */

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
}

if (document.readyState != 'loading') {
  start()
} else {
  document.addEventListener('DOMContentLoaded', start)
}
