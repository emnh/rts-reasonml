type element;

type styleT;

type imageT;

let window: element = [%bs.raw "window"];

let document: element = [%bs.raw "window.document"];

let body: element = [%bs.raw "window.document.body"];

[@bs.get] external iteration : element => int = "iteration";

[@bs.get] external readyState : element => string = "readyState";

[@bs.set] external debug : (element, 'a) => unit = "debug";

[@bs.val] external userAgent : string = "navigator.userAgent";

[@bs.val]
external createElement : string => element = "document.createElement";

[@bs.val]
external appendChild : element => element = "document.body.appendChild";

[@bs.send]
external appendChild2 : (element, element) => element = "appendChild";

[@bs.val]
external removeChild : element => element = "document.body.removeChild";

[@bs.val]
external getElementsByClassName : element => array(element) =
  "getElementsByClassName";

[@bs.send]
external addEventListener : (element, string, Events.event => unit) => unit =
  "addEventListener";

[@bs.get] external getWidth : element => int = "innerWidth";

[@bs.get] external getHeight : element => int = "innerHeight";

[@bs.set] external setWidth : (element, int) => unit = "width";

[@bs.set] external setHeight : (element, int) => unit = "height";

[@bs.set] external setInnerHTML : (element, string) => unit = "innerHTML";

[@bs.get] external getStyle : element => styleT = "style";

[@bs.set] external setMargin : (styleT, string) => unit = "margin";

[@bs.set] external setOverflow : (styleT, string) => unit = "overflow";

[@bs.set] external setPosition : (styleT, string) => unit = "position";

[@bs.set] external setTop : (styleT, string) => unit = "top";

[@bs.set] external setLeft : (styleT, string) => unit = "left";

[@bs.val]
external requestAnimationFrame : (unit => unit) => unit =
  "requestAnimationFrame";

[@bs.val] external setTimeout : (unit => unit, int) => unit = "setTimeout";

[@bs.new] external createImage : unit => imageT = "Image";

[@bs.set] external setSource : (imageT, string) => unit = "src";

/* TODO: 'a = event type */
[@bs.set] external setOnLoad : (imageT, 'a => unit) => unit = "onload";

let onReady = f => {
  let g = (_) => f();
  let isDone = readyState(document) == "complete";
  if (isDone) {
    f();
  } else {
    addEventListener(window, "DOMContentLoaded", g);
  };
};
