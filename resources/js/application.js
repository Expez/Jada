App = Ember.Application.create({
  LOG_TRANSITIONS: true
});

App.Store = DS.Store.extend({
  adapter: DS.RESTAdapter.extend({
    createRecord: function(store, type, record) {
      var data = {};
      var serializer = store.serializerFor(type.typeKey);

      serializer.serializeIntoHash(data, type, record, { includeId: true });
      return this.ajax(this.buildURL(type.typeKey), "POST", { data: data });
    }
  })
});

App.FoodSerializer = DS.RESTSerializer.extend({
  normalize: function(type, hash, property) {
    hash.id = hash.name;
    return this._super(type, hash, property);
  }
});
