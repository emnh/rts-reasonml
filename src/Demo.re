/* externals */
module Math = {
  let pi = [%bs.raw "Math.PI"];
  [@bs.val] external random : unit => float = "Math.random";
};

module Date = {
  [@bs.val] external now : unit => float = "Date.now";
};

/* main app state */
type windowT = {
  mutable width: int,
  mutable height: int
};

type stateT = {window: windowT};

let state: stateT = {
  window: {
    width: 0,
    height: 0
  }
};

/* canvas/context setup */
let canvas = Document.createElement("canvas");

let ctx = Canvas.getContext(canvas, "2d");

Document.appendChild(canvas);

let setCanvasSize = (_) => {
  let width = Document.getWidth(Document.window);
  let height = Document.getHeight(Document.window);
  Document.setWidth(canvas, width);
  Document.setHeight(canvas, height);
  state.window.width = width;
  state.window.height = height;
};

Document.addEventListener(Document.window, "resize", setCanvasSize);

Document.addEventListener(Document.window, "DOMContentLoaded", setCanvasSize);

let start = Date.now();

let _ = ConfigUI.registerCreateHandlers();

let backgroundColor =
  Config.colorConfigVar(["canvas", "background", "color"], (0, 0, 0, 1.0));

let foregroundColor =
  Config.colorConfigVar(["canvas", "foreground", "color"], (0, 0, 0, 1.0));

let rec loop = () => {
  /* let t = Date.now() -. start; */
  let width = state.window.width;
  let height = state.window.height;
  let bgColorString = Color.stringColor(Color.setA(backgroundColor#get(), 1.0));
  let fgColorString = Color.stringColor(Color.setA(foregroundColor#get(), 1.0));
  Canvas.fillStyle(ctx, bgColorString);
  Canvas.fillRect(ctx, 0, 0, width, height);
  Canvas.fillStyle(ctx, fgColorString);
  Canvas.fillRect(ctx, 0, 0, width / 2, height / 2);
  Document.requestAnimationFrame(loop);
};

loop();

Js.log("shader");

Js.log(GLSL.shader);
