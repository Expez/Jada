App.Food = DS.Model.extend({
  name: DS.attr('string'),
  kcal: DS.attr('number'),
  prot: DS.attr('number'),
  fat: DS.attr('number'),
  carbs: DS.attr('number'),
  fiber: DS.attr('number'),
  isCompleted: DS.attr('boolean')
});

App.Food.FIXTURES = [
  {
    id: 1,
    name: 'Foo'
  },
  {
    id: 2,
    name: 'Bar',
  },
  {
    id: 3,
    name: 'Baz'
  }
];
