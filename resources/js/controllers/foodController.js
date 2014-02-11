App.FoodController = Ember.ObjectController.extend({
  actions: {
    edit: function(){
      this.transitionToRoute('food.edit');
    }
  }
});
