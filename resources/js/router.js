App.Router.map(function() {
  this.route("food");
  this.route("log", { path: "/log" });
});

App.FoodRoute = Ember.Route.extend({
  model: function() {
    return this.store.find('food');

    // return [
    //   {
    //     id: 1,
    //     name: 'Foo'
    //   },
    //   {
    //     id: 2,
    //     name: 'Bar',
    //   },
    //   {
    //     id: 3,
    //     name: 'Baz'
    //   }
    // ];
  }
});

App.IndexRoute = Ember.Route.extend({
  model: function() {
    return ['red', 'yellow', 'blue'];
  }
});
