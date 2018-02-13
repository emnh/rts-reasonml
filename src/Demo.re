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

type stateT = {
  window: windowT
};

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

let dg = DatGui.datGUI(DatGui.default);

let addUIColor = (name, value, onChange) => {
  let guiObj = Js.Dict.empty();
  let _ = Js.Dict.set(guiObj, name, value);
  let controller = DatGui.addColorRGBA(dg, guiObj, name);
  DatGui.onColorRGBAChange(controller, onChange);
};

let backgroundColor =
  Config.colorConfigVar(["canvas", "background", "color"], (0, 0, 0, 1.0));

addUIColor("backGroundColor", backgroundColor.get(), v => backgroundColor.set(v));

/*
backgroundColor.registerUpdate(v => {
  Js.log(v);
});
*/

let a = Config.intConfigVar(["configtest"], 3);

let rec loop = () => {
  /* let t = Date.now() -. start; */
  let width = state.window.width;
  let height = state.window.height;
  let (r, g, b, _) = backgroundColor.get();
  let stringColor =
    "rgb("
    ++ string_of_int(r)
    ++ ","
    ++ string_of_int(g)
    ++ ","
    ++ string_of_int(b)
    ++ ")";
  Js.log(stringColor);
  Canvas.fillStyle(ctx, stringColor);
  Canvas.fillRect(ctx, 0, 0, width, height);
  Document.requestAnimationFrame(loop);
};

loop();

Js.log("shader");

Js.log(GLSL.shader);
