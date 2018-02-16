let pi = [%bs.raw "Math.PI"];

[@bs.module "seedrandom"] [@bs.val] external localSeedRandom : unit => (string => unit) = "seedrandom";

[@bs.val] external globalSeedRandom : string => unit = "Math.seedrandom";

[@bs.val] external random : unit => float = "Math.random";

[@bs.val] external sin : float => float = "Math.sin";

[@bs.val] external cos : float => float = "Math.cos";
