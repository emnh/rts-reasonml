type t;

[%%bs.raw
  {|

  /*
var CCapture = require('ccapture.js');

window.capturer = new CCapture( 
  { 
    format: 'ffmpegserver',
    framerate: 24,
    name: 'glacialsyrup',
    extension: '.mp4',
    codec: 'libx264',
    verbose: true,
    display: true,
    timeLimit: 5.0,
    ffmpegArguments: [
      "-preset", "slow",
      "-crf", "18",
      "-pix_fmt", "yuv420p",
      "-movflags", "+faststart"
    ]
  }
);
  */

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
  this.cache = Object.create(null);
}

ObjectWithoutPrototypeCache.prototype.has = function (key) {
  return (key in this.cache);
}

ObjectWithoutPrototypeCache.prototype.get = function (key) {
  return this.cache[key];
}

ObjectWithoutPrototypeCache.prototype.set = function (key, value) {
  this.cache[key] = value;
}

ObjectWithoutPrototypeCache.prototype.get2 = function(key1, key2) {
  var a = this.cache[key1];
  if (typeof a === 'undefined') {
    return a;
  }
  return a[key2];
}

ObjectWithoutPrototypeCache.prototype.set2 = function(key1, key2, value) {
  var a = this.cache[key1];
  if (typeof a === 'undefined') {
    this.cache[key1] = {};
  }
  this.cache[key1][key2] = value;
}

ObjectWithoutPrototypeCache.prototype.get3 = function(key1, key2, key3) {
  var a = this.cache[key1];
  if (typeof a === 'undefined') {
    return a;
  }
  var b = a[key2];
  if (typeof b === 'undefined') {
    return b;
  }
  return b[key3];
}

ObjectWithoutPrototypeCache.prototype.set3 = function(key1, key2, key3, value) {
  var a = this.cache[key1];
  if (typeof a === 'undefined') {
    this.cache[key1] = {};
  }
  var b = this.cache[key1][key2];
  if (typeof b === 'undefined') {
    this.cache[key1][key2] = {};
  }
  this.cache[key1][key2][key3] = value;
}

ObjectWithoutPrototypeCache.prototype.get4 = function(key1, key2, key3, key4) {
  var a = this.cache[key1];
  if (typeof a === 'undefined') {
    return a;
  }
  var b = a[key2];
  if (typeof b === 'undefined') {
    return b;
  }
  var c = b[key3];
  if (typeof c === 'undefined') {
    return c;
  }
  return c[key4];
}

ObjectWithoutPrototypeCache.prototype.set4 = function(key1, key2, key3, key4, value) {
  var a = this.cache[key1];
  if (typeof a === 'undefined') {
    this.cache[key1] = {};
  }
  var b = this.cache[key1][key2];
  if (typeof b === 'undefined') {
    this.cache[key1][key2] = {};
  }
  var c = this.cache[key1][key2][key3];
  if (typeof c === 'undefined') {
    this.cache[key1][key2][key3] = {};
  }
  this.cache[key1][key2][key3][key4] = value;
}

var cacheDefault = {
  create: function create () {
    return new ObjectWithoutPrototypeCache()
  }
}

//
// API
//

  function serialize(obj) {
    if (obj !== Object(obj)) {
      return obj;
    }
    if ("memoizeId" in obj) {
      return obj.memoizeId;
    }
    obj.memoizeId = window.memoizeId++;
    return obj.memoizeId;
    //return JSON.stringify(obj);
  }

  function memoize0(cache, f) {
    var cacheId = "";
    var retval = cache.get(cacheId);
    if (typeof retval === 'undefined') {
      retval = f();
      cache.set(cacheId, retval);
      //console.log("memoize0: ", retval);
    }
    return retval;
  };

  function memoize1(cache, f, a) {
    var sa = serialize(a);
    var retval = cache.get(sa);
    if (typeof retval === 'undefined') {
      retval = f(a);
      cache.set(sa, retval);
      //console.log("memoize1: ", retval, sa);
    }
    return retval;
  };

  function memoize2(cache, f, a, b) {
    var sa = serialize(a);
    var sb = serialize(b);
    var retval = cache.get2(sa, sb);
    if (typeof retval === 'undefined') {
      retval = f(a, b);
      cache.set2(sa, sb, retval);
      //console.log("memoize2: ", retval, sa, sb);
    }
    return retval;
  };

  function memoize3(cache, f, a, b, c) {
    var sa = serialize(a);
    var sb = serialize(b);
    var sc = serialize(c);
    var retval = cache.get3(sa, sb, sc);
    if (typeof retval === 'undefined') {
      retval = f(a, b, c);
      cache.set3(sa, sb, sc, retval);
      //console.log("memoize3: ", retval, sa, sb, sc);
    }
    return retval;
  };

  function memoize4(cache, f, a, b, c, d) {
    var sa = serialize(a);
    var sb = serialize(b);
    var sc = serialize(c);
    var sd = serialize(d);
    var retval = cache.get4(sa, sb, sc, sd);
    if (typeof retval === 'undefined') {
      retval = f(a, b, c, d);
      cache.set4(sa, sb, sc, sd, retval);
      //console.log("memoize4: ", retval, sa, sb, sc, sd);
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
