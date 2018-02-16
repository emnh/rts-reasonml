type t;

[%%bs.raw
  {|
  window.memoize = function(argCount, f) {
    var cache = {};
    var gSubMemoize;
    switch (argCount) {
      case 0:
        gSubMemoize = function() {
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
        gSubMemoize = function(a) {
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
        gSubMemoize = function(a, b) {
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
      case 3:
        gSubMemoize = function(a, b, c) {
          var cacheId = JSON.stringify(arguments);
          var retval;
          if (cacheId in cache) {
            return cache[cacheId];
          } else {
            retval = f(a, b, c);
          }
          cache[cacheId] = retval;
          return retval;
        };
        break;
      case 4:
        gSubMemoize = function(a, b, c, d) {
          var cacheId = JSON.stringify(arguments);
          var retval;
          if (cacheId in cache) {
            return cache[cacheId];
          } else {
            retval = f(a, b, c, d);
          }
          cache[cacheId] = retval;
          return retval;
        };
        break;
      default:
        throw "Unsupported argument count to memoize!";
        break;
    };
    return gSubMemoize;
  };
  
  window.partialMemoize = function(argCount, argIndices, f) {
    var cache = {};
    var getArgs = function(args) {
      var newArgs = [];
      for (var i = 0; i < argIndices.length; i++) {
        newArgs.push(args[argIndices[i]]);
      }
      return JSON.stringify(newArgs);
    };
    var gSubMemoize;
    switch (argCount) {
      case 0:
        gSubMemoize = function() {
          var cacheId = getArgs(arguments);
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
        gSubMemoize = function(a) {
          var cacheId = getArgs(arguments);
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
        gSubMemoize = function(a, b) {
          var cacheId = getArgs(arguments);
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
      case 3:
        gSubMemoize = function(a, b, c) {
          var cacheId = getArgs(arguments);
          var retval;
          if (cacheId in cache) {
            return cache[cacheId];
          } else {
            retval = f(a, b, c);
          }
          cache[cacheId] = retval;
          return retval;
        };
        break;
      default:
        throw "Unsupported argument count to memoize!";
        break;
    };
    return gSubMemoize;
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
