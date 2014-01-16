var ws = new WebSocket("ws://127.0.0.1:8080/websocket");
ws.onmessage = function(event) {
  var message = JSON.parse(event.data);
  var action = message.action;
  var recipient = message.recipient;
  var value = message.value;
  if(action === "replace") {
    $(recipient).val(value);
  }
};

$('#weight').keypress(function (e) {
  if (e.which == 13) {
    e.preventDefault();
    var sender = "#weight-form";
    var val = $("#weight").val();
    if(!val){
      return;
    }
    console.log("sending val: " + val);
    ws.send(JSON.stringify({fn: "weight", args: val, sender: sender}));
    return false;    //<---- Add this line
  }
});
ws.onopen = function() {
};
