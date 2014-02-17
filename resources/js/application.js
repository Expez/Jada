App = Ember.Application.create({
  LOG_TRANSITIONS: true
});

App.FoodSerializer = DS.RESTSerializer.extend({
  normalize: function(type, hash, property) {
    hash.id = hash.name;
    return this._super(type, hash, property);
  }
});
