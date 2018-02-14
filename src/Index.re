[%%bs.raw {|
  var App = require("./Demo.bs.js");

  if (window.iteration === undefined) {
    window.iteration = 0;
  }
  window.iteration++;

  App.main();

  if (module.hot) {
    module.hot.accept(() => {
      const NextApp = require('./Demo.bs.js').default;
      console.log("iteration: ", window.iteration);
      NextApp.main(window.iteration);
    });
  }
|}];
