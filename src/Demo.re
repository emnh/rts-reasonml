/* externals */
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

exception NoGL;

exception No2D;

exception NoProgram;

let counter = ref(0);

let countInvocations =
  Memoize.memoize(
    1,
    a => {
      counter := counter^ + 1;
      ("count", a, counter^);
    }
  );

countInvocations(0);

/* canvas/context setup */
let main = (_) => {
  /* Clear doc in case of hot reloading. Didn't work with dat.gui. */
  /*
   Document.setInnerHTML(Document.body, "");
   */
  Document.setMargin(Document.getStyle(Document.body), "0px");
  Document.setOverflow(Document.getStyle(Document.body), "hidden");
  let canvas = Document.createElement("canvas");
  let showError = msg => {
    let body = Document.body;
    Document.setInnerHTML(body, "<h1 style='color: red;'>" ++ msg ++ "</h1>");
  };
  let _ = Document.appendChild(canvas);
  let gl =
    switch (Js.Nullable.to_opt(WebGL2.getContext(canvas, "webgl2"))) {
    | Some(gl) => gl
    | None =>
      showError("No WebGL2!");
      raise(NoGL);
    };
  let getShaderProgram =
    Memoize.memoize(
      2,
      (fg, bg) => {
        let (uniforms, programSource) =
          ShaderExample.makeProgramSource(fg, bg);
        let vertexShaderSource = programSource.vertexShader;
        let fragmentShaderSource = programSource.fragmentShader;
        Js.log("Vertex shader:");
        Js.log(vertexShaderSource);
        let vertexShader =
          WebGL2.createShader(
            gl,
            WebGL2.getVERTEX_SHADER(gl),
            vertexShaderSource
          );
        Js.log("Fragment shader:");
        Js.log(fragmentShaderSource);
        let fragmentShader =
          WebGL2.createShader(
            gl,
            WebGL2.getFRAGMENT_SHADER(gl),
            fragmentShaderSource
          );
        let program =
          switch (vertexShader, fragmentShader) {
          | (Some(vs), Some(fs)) => WebGL2.createProgram(gl, vs, fs)
          | _ => None
          };
        (uniforms, program);
      }
    );
  let run = (time, geometryType, fg, bg) => {
    let geof =
      switch geometryType {
      | "Box" => Three.createBoxGeometry
      | "Sphere" => Three.createSphereGeometry
      | _ => Three.createBoxGeometry
      };
    switch (getShaderProgram(fg, bg)) {
    | (uniforms, Some(p)) =>
      WebGL2.testProgram(
        gl,
        p,
        uniforms,
        state.window.width,
        state.window.height,
        time,
        fg,
        bg,
        geof,
        Three.getObjectMatrix,
        Three.getViewMatrices
      )
    | (_, None) => raise(NoProgram)
    };
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
  setCanvasSize();
  Document.addEventListener(
    Document.window,
    "DOMContentLoaded",
    setCanvasSize
  );
  let start = Date.now();
  ConfigUI.init();
  let backgroundColor =
    Config.colorConfigVar(
      ["canvas", "background", "color"],
      (0, 0, 0, 1.0),
      ()
    );
  let foregroundColor =
    Config.colorConfigVar(
      ["canvas", "foreground", "color"],
      (0, 0, 0, 1.0),
      ()
    );
  let geometryType =
    Config.stringConfigVar(
      ["object", "geometry"],
      "Box",
      ~choices=Config.Choices([|"Box", "Sphere"|]),
      ()
    );
  let startIteration = Document.iteration(Document.window);
  let rec loop = () => {
    let t = Date.now() -. start;
    run(
      t /. 1000.0,
      geometryType#get(),
      foregroundColor#get(),
      backgroundColor#get()
    );
    let currentIteration = Document.iteration(Document.window);
    if (currentIteration == startIteration) {
      Document.requestAnimationFrame(loop);
    } else {
      let _ = Document.removeChild(canvas);
      Js.log(
        "exiting render loop "
        ++ string_of_int(startIteration)
        ++ " due to hot reload"
      );
    };
  };
  loop();
  Js.log(__LOC__);
  () => {
    Js.log("destroying last app generation");
    ConfigUI.destroy();
  };
};
