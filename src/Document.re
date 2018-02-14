type element;

let window: element = [%bs.raw "window"];

let body: element = [%bs.raw "window.document.body"];

[@bs.val]
external createElement : string => element = "document.createElement";

[@bs.val]
external appendChild : element => element = "document.body.appendChild";

[@bs.send]
external addEventListener : (element, string, Events.event => unit) => unit =
  "addEventListener";

[@bs.get] external getWidth : element => int = "innerWidth";

[@bs.get] external getHeight : element => int = "innerHeight";

[@bs.set] external setWidth : (element, int) => unit = "width";

[@bs.set] external setHeight : (element, int) => unit = "height";

[@bs.set] external setInnerHTML : (element, string) => unit = "innerHTML";

[@bs.val]
external requestAnimationFrame : (unit => unit) => unit =
  "requestAnimationFrame";
