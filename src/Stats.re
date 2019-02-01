type statsModuleT;

type statsT;

[@bs.module "stats.js"] external statsModule : statsModuleT = "";

/*
 [@bs.new] [@bs.module "stats.js"]
 external createStats : unit => statsT = "Stats";
 */
let createStats: [@bs] unit => statsT = {
  let _ = statsModule;
  [%bs.raw {| function() { return new StatsJs(); } |}];
};

[@bs.send] external beginStats : statsT => unit = "begin";

[@bs.send] external endStats : statsT => unit = "end";

[@bs.get] external dom : statsT => Document.element = "dom";
