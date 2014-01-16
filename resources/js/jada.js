var ws = new WebSocket("ws://127.0.0.1:8080/websocket");
ws.onmessage = function(event) {
  var message = JSON.parse(event.data);
  var html = JSON.parse(message.html);
  console.log("html = ", html);
  var recipient = message.recipient;
  $(recipient).html(html);
};

$("#box").submit(function(event) {
  event.preventDefault();
  var sender = "#box";
  var val = $("#input").val();
  ws.send(JSON.stringify({fn: "box", args: val, sender: sender}));
});
ws.onopen = function() {
};
