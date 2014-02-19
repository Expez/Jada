App.FoodEditController = Ember.ObjectController.extend({
  actions: {
    save: function(){
      var controller = this;
      function transitionToFood(food) {
        controller.transitionToRoute('food', food);
      }
      function failure(reason) {
        console.log("Failure in foodEditController.js");
        console.log(reason);
      }
      this.get('model').save().then(transitionToFood).catch(failure);
    }
  }
});
