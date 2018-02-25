let pi = [%bs.raw "Math.PI"];

[@bs.module "seedrandom"] [@bs.val]
external localSeedRandom : (unit, string) => unit = "seedrandom";

[@bs.val]
external globalSeedRandom :
  (string, [@bs.as {json|{ "entropy": true }|json}] _) => unit =
  "Math.seedrandom";

[@bs.val] external random : unit => float = "Math.random";

[@bs.val] external sqrt : float => float = "Math.sqrt";

[@bs.val] external sin : float => float = "Math.sin";

[@bs.val] external cos : float => float = "Math.cos";

[@bs.val] external floor : float => float = "Math.floor";

let getRandomInt = max =>
  int_of_float(floor(random() *. floor(float_of_int(max))));
