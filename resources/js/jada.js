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
    console.log("1");
    var sender = "#weight-form";
    console.log("2");
    var val = $("#weight").val();
    console.log("3");
    if(!val){
      console.log("4");
      return;
    }
    console.log("sending val: " + val);
    ws.send(JSON.stringify({fn: "weight", args: val, sender: sender}));
    return false;    //<---- Add this line
  }
});
ws.onopen = function() {
};
