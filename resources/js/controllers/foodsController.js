App.FoodsController = Ember.ArrayController.extend({
  sortProperties: ['name'],
  sortAscending: false,
  actions: {
    create: function(){
      this.transitionToRoute('foods.create');
    }
  }
});
