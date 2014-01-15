(function () {
  var ws = new WebSocket("ws://127.0.0.1:8080/websocket");
  ws.onmessage = function(event) {
    console.log(event.data);
  };
  ws.onopen = function() {
    var msg = JSON.stringify({fn: "foo", args: [1, 2, 3]});
    ws.send(msg);
  };

}());
