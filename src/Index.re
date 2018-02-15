[%%bs.raw {|
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
|}];
