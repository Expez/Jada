Meteor.subscribe("log");
Meteor.subscribe("foods");
Meteor.subscribe("macros");

// Template.foods.foods = function () {
//   return Foods.find({}, {sort: {name: 1}});
// };

// Template.addFood.events = {
//   'submit': function() {

//   }
// };

// Template.displayFood = function() {
//   return Foods.findOne(Session.get("selected"));
// };
