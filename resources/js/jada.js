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
    return false;
  }
});

ws.onopen = function() {
};

$('#food-form').submit(function(e) {
  var fields = ["food-name", "food-kcal", "food-prot", "food-fat",
                         "food-carbs", "food-fiber"];
  var values = inputsToArray.apply(null, fields);
  var f = _.partial(dropPrefix, "food-");
  var keys = fields.map(f);
  var argMap = createMap(keys, values);
  ws.send(JSON.stringify({fn: "add-food", args: argMap, sender: "#food-form"}));
});

// vararg input with ids of the inputs fields
// returns a map of {id: value ...}
var inputsToArray = function(inputFields) {
  var values = [];
  for (var i = 0; i < arguments.length; i++) {
    values.push($("#" + arguments[i]).val());
  }
  return values;
};
// Drops `prefix' from 's'
var dropPrefix = function (prefix, s) {
  return s.replace(prefix, '');
};

var createMap = function(keys, values) {
  var map = {};
  for (var i = 0; i < keys.length; i++) {
    map[keys[i]] = values[i];
  }
  return map;
};
