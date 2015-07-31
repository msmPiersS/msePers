class Unit {

  // Entity is used as node or edge type, for different classifications
  //    i.e. 'person', 'game', 'road', etc.
  constructor(entity, properties) {

    this.entity = entity + '';

    this.load(properties || {});

  }

  // load properties (id, name, age, etc.) from an object
  load(properties) {

    let p = Object.create(null);

    Object.keys(properties).forEach(function(v) {

      p[v] = properties[v];

    });

    this.properties = p;

    return this;

  }

  set(property, value) {

    return this.properties[property] = value;

  }

  unset(property) {

    return delete this.properties[property];

  }

  has(property) {

    return Object.prototype.hasOwnProperty.call(this.properties, property);

  }

  get(property) {

    return this.properties[property];

  }

  toString() {

    return [this.constructor.name, ' (', this.entity, ' ', JSON.stringify(this.properties) ,')'].join('');

  }

}