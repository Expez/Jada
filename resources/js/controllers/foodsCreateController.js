App.FoodsCreateController = Ember.ObjectController.extend({
  actions: {
    save: function(){
      var food = this.store.createRecord('food', this.get('model'));
      console.log(food);
      food.save();

      this.transitionToRoute('food', food);
    }
  }
});
