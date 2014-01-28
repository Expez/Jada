var assert = require('assert');

suite('Foods', function() {
  // test('on the server', function(done, server) {
  //   server.eval(function () {
  //     Foods.insert({name: "asdf"});
  //     var foods = Foods.find().fetch();
  //     emit('foods', foods);
  //   });

  //   server.once('foods', function(foods) {
  //     assert.equal(foods.length, 1);
  //     done();
  //   });
  // });

  // test('addfood', function (done, server, client) {
  //   server.eval(function() {
  //     console.log("hi thar!");
  //     console.log(Foods.find().fetch());
  //     Foods.find().observe({
  //       added: addedFood
  //     });

  //     function addedFood(food) {
  //       emit('food', food);
  //     }
  //   }).once('food', function(food) {
  //     assert(true);
  //     done();
  //   });
  //   // var f1 = {name: "asdf", fat: 1, kcal: 2, prot: 3, carbs: 4, fiber: 5};
  //   // var f2 = Foods.find({name: f1.name}).fetch();
  //   // assert.strictEqual(f1, _omit(f2, '_id'));

  // client.eval(function () {
  //   var f1 = {name: "asdf", fat: 1, kcal: 2, prot: 3, carbs: 4, fiber: 5};
  //   addFood(f1);
  // });

  // });

  test('constantlyOne', function(done, server, client) {
    client.eval(function () {
      done();
      // assert.ok(true);
      // var ret = constantlyOne();
    });
  });
});
