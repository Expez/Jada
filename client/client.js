Meteor.subscribe("log");
Meteor.subscribe("foods");
Meteor.subscribe("macros");

Template.hello.greeting = function () {
  return "Welcome to jada.";
};

Template.hello.events({
  'click input' : function () {
    // template data, if any, is available in 'this'
    if (typeof console !== 'undefined')
      console.log("You pressed the button");
  }
});
