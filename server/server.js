Meteor.publish("log", function () {
  return Meteor.log.find({_id: this.userid});
});

Meteor.publish("foods", function () {
  return Meteor.foods.find({_id: this.userid});
});

Meteor.publish("macros", function () {
  return Meteor.macros.find({_id: this.userid});
});

Meteor.startup(function () {
  // code to run on server at startup
});
