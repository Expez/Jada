App.Router.map(function() {
  // this.route("food");
  this.resource('foods', function(){
    this.resource('food', { path:'/:id' }, function(){
      this.route('edit');
    });
    this.route('create');
  });
  this.route("log", { path: "/log" });
});

App.FoodsRoute = Ember.Route.extend({
  model: function() {
    return this.store.find('food');
  }
});

App.FoodRoute = Ember.Route.extend({
  model: function(params) {
    return this.store.find('food', params.id);
  }
});

App.IndexRoute = Ember.Route.extend({
  model: function() {
    return ['red', 'yellow', 'blue'];
  }
});