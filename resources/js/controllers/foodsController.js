App.FoodsController = Ember.ArrayController.extend({
  sortProperties: ['name'],
  sortAscending: false,
    create: function(){
      this.transitionToRoute('foods.create');
    }
});
