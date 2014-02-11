App.FoodEditController = Ember.ObjectController.extend({
  actions: {
    save: function(){
      var food = this.get('model');
      food.save();
      this.transitionToRoute('food', food);
    }
  }
});
