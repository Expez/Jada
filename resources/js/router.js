App.Router.map(function() {
  this.route("food");
  this.route("log", { path: "/log" });
});

App.FoodRoute = Ember.Route.extend({
  model: function() {
    return this.store.find('food');
  }
});

App.IndexRoute = Ember.Route.extend({
  model: function() {
    return ['red', 'yellow', 'blue'];
  }
});
