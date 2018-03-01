type t;

[%%bs.raw
  {|

//
// Main
//

function memoize (fn, options) {
  var cache = options && options.cache
    ? options.cache
    : cacheDefault

  var serializer = options && options.serializer
    ? options.serializer
    : serializerDefault

  var strategy = options && options.strategy
    ? options.strategy
    : strategyDefault

  return strategy(fn, {
    cache: cache,
    serializer: serializer
  })
}

//
// Strategy
//

function isPrimitive (value) {
  return value == null || typeof value === 'number' || typeof value === 'boolean' // || typeof value === "string" 'unsafe' primitive for our needs
}

function monadic (fn, cache, serializer, arg) {
  console.log("MONADIC f");
  var cacheKey = isPrimitive(arg) ? arg : serializer(arg)

  var computedValue = cache.get(cacheKey)
  if (typeof computedValue === 'undefined') {
    computedValue = fn.call(this, arg)
    cache.set(cacheKey, computedValue)
  }

  return computedValue
}

function variadic (fn, cache, serializer) {
  console.log("HELO f");
  var args = Array.prototype.slice.call(arguments, 3)
  var cacheKey = serializer(args)

  var computedValue = cache.get(cacheKey)
  if (typeof computedValue === 'undefined') {
    computedValue = fn.apply(this, args)
    cache.set(cacheKey, computedValue)
  }

  return computedValue
}

function assemble (fn, context, strategy, cache, serialize) {
  return strategy.bind(
    context,
    fn,
    cache,
    serialize
  )
}

function strategyDefault (fn, options) {
  var strategy = fn.length === 1 ? monadic : variadic

  return assemble(
    fn,
    this,
    strategy,
    options.cache.create(),
    options.serializer
  )
}

function strategyVariadic (fn, options) {
  var strategy = variadic

  return assemble(
    fn,
    this,
    strategy,
    options.cache.create(),
    options.serializer
  )
}

function strategyMonadic (fn, options) {
  var strategy = monadic

  return assemble(
    fn,
    this,
    strategy,
    options.cache.create(),
    options.serializer
  )
}

//
// Serializer
//

function serializerDefault () {
  return JSON.stringify(arguments)
}

//
// Cache
//

function ObjectWithoutPrototypeCache () {
  this.cache = Object.create(null)
}

ObjectWithoutPrototypeCache.prototype.has = function (key) {
  return (key in this.cache)
}

ObjectWithoutPrototypeCache.prototype.get = function (key) {
  return this.cache[key]
}

ObjectWithoutPrototypeCache.prototype.set = function (key, value) {
  this.cache[key] = value
}

var cacheDefault = {
  create: function create () {
    return new ObjectWithoutPrototypeCache()
  }
}

//
// API
//

  function memoize0(cache, f) {
    var args = [];
    var cacheId = JSON.stringify(args);
    var retval = cache.get(cacheId);
    if (typeof retval === 'undefined') {
      retval = f();
      cache.set(cacheId, retval);
    }
    return retval;
  };

  function memoize1(cache, f, a) {
    var args = [a];
    var cacheId = JSON.stringify(args);
    var retval = cache.get(cacheId);
    if (typeof retval === 'undefined') {
      retval = f(a);
      cache.set(cacheId, retval);
    }
    return retval;
  };

  function memoize2(cache, f, a, b) {
    var args = [a, b];
    var cacheId = JSON.stringify(args);
    var retval = cache.get(cacheId);
    if (typeof retval === 'undefined') {
      retval = f(a, b);
      cache.set(cacheId, retval);
    }
    return retval;
  };

  function memoize3(cache, f, a, b, c) {
    var args = [a, b, c];
    var cacheId = JSON.stringify(args);
    var retval = cache.get(cacheId);
    if (typeof retval === 'undefined') {
      retval = f(a, b, c);
      cache.set(cacheId, retval);
    }
    return retval;
  };

  function memoize4(cache, f, a, b, c, d) {
    var args = [a, b, c, d];
    var cacheId = JSON.stringify(args);
    var retval = cache.get(cacheId);
    if (typeof retval === 'undefined') {
      retval = f(a, b, c, d);
      cache.set(cacheId, retval);
    }
    return retval;
  };

  window.partialMemoize0 = function(f) {
    var cache = new ObjectWithoutPrototypeCache();
    return memoize0.bind(this, cache, f);
  };

  window.partialMemoize1 = function(f) {
    var cache = new ObjectWithoutPrototypeCache();
    return memoize1.bind(this, cache, f);
  };

  window.partialMemoize2 = function(f) {
    var cache = new ObjectWithoutPrototypeCache();
    return memoize2.bind(this, cache, f);
  };

  window.partialMemoize3 = function(f) {
    var cache = new ObjectWithoutPrototypeCache();
    return memoize3.bind(this, cache, f);
  }

  window.partialMemoize4 = function(f) {
    var cache = new ObjectWithoutPrototypeCache();
    return memoize4.bind(this, cache, f);
  }
  var App = require("./Demo.bs.js");

  if (window.iteration === undefined) {
    window.iteration = 0;
  }
  window.iteration++;

  if (window.memoizeId === undefined) {
    window.memoizeId = 0;
  }

  if (window.destroy !== undefined) {
    window.destroy();
  }
  window.destroy = App.main();

  if (module.hot) {
    module.hot.accept(() => {
      destroy();
      const NextApp = require('./Demo.bs.js').default;
      console.log("iteration: ", window.iteration);
      if (window.destroy !== undefined) {
        window.destroy();
      }
      window.destroy = NextApp.main();
    });
  }
|}
];
