(function () {
  var httpRequest;
  if (window.XMLHttpRequest) { // Mozilla, Safari, ...
    httpRequest = new XMLHttpRequest();
  } else if (window.ActiveXObject) { // IE 8 and older
    httpRequest = new ActiveXObject("Microsoft.XMLHTTP");
  }
  httpRequest.onreadystatechange = function() {
    if (httpRequest.readyState === 4) {
      console.log("state change");
    } else {
      console.log("Something went wrong");
    }
  };
  httpRequest.open('GET', 'localhost:3000/val', true);
  httpRequest.send(null);
}());
