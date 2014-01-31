Jada.Router.map(function() {
  // put your routes here
});

Jada.IndexRoute = Ember.Route.extend({
  model: function() {
    return ['red', 'yellow', 'blue'];
  }
});
