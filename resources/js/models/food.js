App.Food = DS.Model.extend({
  name: DS.attr('string'),
  kcal: DS.attr('number'),
  prot: DS.attr('number'),
  fat: DS.attr('number'),
  carbs: DS.attr('number'),
  fiber: DS.attr('number')
});
