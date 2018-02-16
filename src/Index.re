type t;

[%%bs.raw
  {|
  window.memoize = function(argCount, f) {
    var cache = {};
    var g;
    switch (argCount) {
      case 0:
        g = function() {
          var cacheId = JSON.stringify(arguments);
          var retval;
          if (cacheId in cache) {
            return cache[cacheId];
          } else {
            retval = f();
          }
          cache[cacheId] = retval;
          return retval;
        };
        break;
      case 1:
        g = function(a) {
          var cacheId = JSON.stringify(arguments);
          var retval;
          if (cacheId in cache) {
            return cache[cacheId];
          } else {
            retval = f(a);
          }
          cache[cacheId] = retval;
          return retval;
        };
        break;
      case 2:
        g = function(a, b) {
          var cacheId = JSON.stringify(arguments);
          var retval;
          if (cacheId in cache) {
            return cache[cacheId];
          } else {
            retval = f(a, b);
          }
          cache[cacheId] = retval;
          return retval;
        };
        break;
    };
    return g;
  };

  var App = require("./Demo.bs.js");

  if (window.iteration === undefined) {
    window.iteration = 0;
  }
  window.iteration++;

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
