App.LogEntry = DS.Model.extend({
  weight: DS.attr('number'),
  date: DS.attr('date'),
  plan: DS.attr(),
  bmr: DS.attr('number'),
  kind: DS.attr('string')
});
