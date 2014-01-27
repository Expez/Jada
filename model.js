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
