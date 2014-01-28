Foods = new Meteor.Collection("foods");
Macros = new Meteor.Collection("macros");
Log = new Meteor.Collection("log");

// Only the owner can modify a collection.  Direct insertion is
// disallowed
var rules = {
  insert: function (userId, doc) {
    return false; // no cowboy inserts
  },
  update: function (userId, doc, fields, modifier) {
    if (userId !== doc.owner)
      return false;

    return true;
  },
  remove: function (userId, doc) {
    return doc.owner === userId;
  }
};

Foods.allow(rules);
Macros.allow(rules);
Log.allow(rules);

var NonEmptyString = Match.Where(function (x) {
  check(x, String);
  return x.length !== 0;
});

var NonNegativeNumber = Match.Where(function (x) {
  check(x, Number);
  return x >= 0;
});

addFood = function (food) {
  var id = Random.id();
  Meteor.call('addFood', _.extend({ _id: id }, food));
  return id;
};

constantlyOne = function() {
  return Meteor.call('constantlyOne');
};


Meteor.methods({
  addFood: function(food) {
    check(food, {
      _id: NonNegativeNumber,
      name: NonEmptyString,
      kcal: NonNegativeNumber,
      fat: NonNegativeNumber,
      prot: NonNegativeNumber,
      carbs: NonNegativeNumber,
      fiber: NonNegativeNumber});
    if (! this.userId)
      throw new Meteor.Error(403, "You must be logged in");
    Foods.insert({
        _id: food._id,
        name: food.name,
        kcal: food.kcal,
        fat: food.fat,
        prot: food.prot,
        carbs: food.carbs,
        fiber: food.fiber});
  },
  constantlyOne: function() {
   return 1;
  }
});
