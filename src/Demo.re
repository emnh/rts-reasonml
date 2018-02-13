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

let dg = DatGui.datGUI(DatGui.default);

let uiFolders = Js.Dict.empty();

/* Create Dat.GUI folders if they don't exist */
let createFolders = (var: Config.configVar('a)) => {
  let rec createFolder = path => {
    let p = var.pathStr(path);
    let folder =
      switch (Js.Dict.get(uiFolders, p)) {
      | Some(folder) => folder
      | None =>
        let f =
          switch path {
          | [] => dg
          | [head, ...tail] =>
            let folder = createFolder(tail);
            DatGui.addFolder(folder, head);
          };
        let _ = Js.Dict.set(uiFolders, p, f);
        f;
      };
    folder;
  };
  createFolder(List.tl(List.rev(var.path)));
};

let addUIColor = (var: Config.configVar(Config.rgbaT)) => {
  let guiObj = Js.Dict.empty();
  let name = List.hd(List.rev(var.path));
  let _ = Js.Dict.set(guiObj, name, var.get());
  let folder = createFolders(var);
  let controller = DatGui.addColorRGBA(folder, guiObj, name);
  DatGui.onColorRGBAChange(controller, v => {
    let (r, g, b, a) = v;
    let r = r + 0;
    let g = g + 0;
    let b = b + 0;
    var.set((r, g, b, a));
  });
};

let backgroundColor =
  Config.colorConfigVar(["canvas", "background", "color"], (0, 0, 0, 1.0));

addUIColor(backgroundColor);

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
