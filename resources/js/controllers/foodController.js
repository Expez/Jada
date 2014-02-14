App.FoodController = Ember.ObjectController.extend({
  actions: {
    edit: function(){
      this.transitionToRoute('food.edit');
    },
    delete: function(){
      var food = this.get('model');
      food.deleteRecord();
      food.save();
      this.transitionToRoute('foods');
    }
  }
});
