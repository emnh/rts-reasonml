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

Document.setMargin(Document.getStyle(Document.body), "0px");
Document.setOverflow(Document.getStyle(Document.body), "hidden");

let canvas = Document.createElement("canvas");

let showError = msg => {
  let body = Document.body;
  Document.setInnerHTML(body, "<h1 style='color: red;'>" ++ msg ++ "</h1>");
};

exception NoGL;

exception No2D;

exception NoProgram;

Document.appendChild(canvas);

let gl =
  switch (Js.Nullable.to_opt(WebGL2.getContext(canvas, "webgl2"))) {
  | Some(gl) => gl
  | None =>
    showError("No WebGL2!");
    raise(NoGL);
  };

let vertexShader =
  WebGL2.createShader(
    gl,
    WebGL2.getVERTEX_SHADER(gl),
    WebGL2.exampleVertexShader
  );

let fragmentShader =
  WebGL2.createShader(
    gl,
    WebGL2.getFRAGMENT_SHADER(gl),
    WebGL2.exampleFragmentShader
  );

let program =
  switch (vertexShader, fragmentShader) {
  | (Some(vs), Some(fs)) => WebGL2.createProgram(gl, vs, fs)
  | _ => None
  };

let run = (time) => {
  let _ =
    switch program {
    | Some(p) =>
      WebGL2.testProgram(gl, p, state.window.width, state.window.height, time)
    | None => raise(NoProgram)
    };
  ();
};

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

/*
Document.addEventListener(Document.window, "DOMContentLoaded", run);
*/

let start = Date.now();

let _ = ConfigUI.registerCreateHandlers();

let backgroundColor =
  Config.colorConfigVar(["canvas", "background", "color"], (0, 0, 0, 1.0));

let foregroundColor =
  Config.colorConfigVar(["canvas", "foreground", "color"], (0, 0, 0, 1.0));

let rec loop = () => {
  let t = Date.now() -. start;
  let width = state.window.width;
  let height = state.window.height;
  let bgColorString =
    Color.stringColor(Color.setA(backgroundColor#get(), 1.0));
  let fgColorString =
    Color.stringColor(Color.setA(foregroundColor#get(), 1.0));
	run(t /. 1000.0);
  Document.requestAnimationFrame(loop);
};

loop();

Js.log("shader");

Js.log(GLSL.shader);
