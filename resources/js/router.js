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

App.FoodEditRoute = Ember.Route.extend({
  model: function(){
    return this.modelFor('food');
  }
});

App.IndexRoute = Ember.Route.extend({
  model: function() {
    return ['red', 'yellow', 'blue'];
  }
});

App.FoodsCreateRoute = Ember.Route.extend({
  model: function(){
    return this.store.createRecord('food', {});
  },

  renderTemplate: function(){
    this.render('food.edit', {
      controller: 'foodsCreate'
    });
  }
});
