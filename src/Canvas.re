type context;

[@bs.send]
external getContext : (Document.element, string) => context = "getContext";

[@bs.send] external beginPath : context => unit = "beginPath";

[@bs.set] external fillStyle : (context, string) => unit = "fillStyle";

[@bs.set] external strokeStyle : (context, string) => unit = "strokeStyle";

[@bs.send] external fill : context => unit = "fill";

[@bs.send] external stroke : context => unit = "stroke";

[@bs.send]
external fillRect : (context, int, int, int, int) => unit = "fillRect";

[@bs.send]
external clearRect : (context, float, float, float, float) => unit =
  "clearRect";

[@bs.send]
external arc : (context, float, float, float, float, float, bool) => unit =
  "arc";
