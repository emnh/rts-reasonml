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

let getShaderProgram =
  Memoize.partialMemoize3((gl, fg, bg) => {
    /*
     let (uniforms, programSource) = ShaderExample.makeProgramSource(fg, bg);
     */
    let (uniforms, programSource) = WaterRenderer.makeProgramSource();
    let vertexShaderSource = programSource.vertexShader;
    let fragmentShaderSource = programSource.fragmentShader;
    let vertexShader =
      WebGL2Util.createShader(
        gl,
        WebGL2.getVERTEX_SHADER(gl),
        vertexShaderSource
      );
    switch vertexShader {
    | Some(_) => ()
    | None =>
      Js.log("Vertex shader:");
      Js.log(MyString.lineNumbers(vertexShaderSource));
    };
    let fragmentShader =
      WebGL2Util.createShader(
        gl,
        WebGL2.getFRAGMENT_SHADER(gl),
        fragmentShaderSource
      );
    switch fragmentShader {
    | Some(_) => ()
    | None =>
      Js.log("Fragment shader:");
      Js.log(MyString.lineNumbers(fragmentShaderSource));
    };
    let program =
      switch (vertexShader, fragmentShader) {
      | (Some(vs), Some(fs)) => WebGL2Util.createProgram(gl, vs, fs)
      | _ => None
      };
    (uniforms, program);
  });

let setupDocument = () => {
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
    | Some(gl) =>
      Memoize.setMemoizeId(gl);
      gl;
    | None =>
      showError("No WebGL2Util!");
      raise(NoGL);
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
  ConfigUI.init();
  (canvas, gl);
};

let getGeometryAndBuffers =
  Memoize.partialMemoize3((gl, program, gt) => {
    let geometry =
      switch gt {
      | "Box" => Three.createBoxGeometry()
      | "Sphere" => Three.createSphereGeometry()
      | "Plane" => Three.createPlaneGeometry()
      | _ => Three.createBoxGeometry()
      };
    let buffers = WebGL2Util.createBuffers(gl, geometry);
    let vao = WebGL2Util.createAttributes(gl, program, buffers);
    (geometry, buffers, vao);
  });

let renderObj =
    (gl, program, buffers, vao, size, pos, rot, width, height, time, uniforms) => {
  let (rx, ry, rz) = rot;
  let obj: Three.objectTransformT =
    Three.getObjectMatrix(
      pos,
      size,
      (
        Math.sin(time) *. 2.0 *. Math.pi +. rx,
        Math.sin(0.35 *. time) *. 2.0 *. Math.pi +. ry,
        Math.sin(0.73 *. time) *. 2.0 *. Math.pi +. rz
      )
    );
  let viewMatrices: Three.viewTransformT =
    Three.getViewMatrices(obj.matrixWorld, width, height);
  let uniformBlock =
    WebGL2Util.computeUniformBlock(
      gl,
      time,
      width,
      height,
      viewMatrices.modelViewMatrix,
      viewMatrices.projectionMatrix,
      uniforms
    );
  WebGL2Util.renderObject(gl, program, buffers, vao, uniformBlock);
};

let seedrandom = Math.localSeedRandom();

let getPosition =
  Memoize.partialMemoize3((_, _, spread) => {
    let f = () => (Math.random() -. 0.5) *. 2.0 *. spread;
    let (rx, ry, rz) = (f(), f(), (-20.0));
    (rx, ry, rz);
  });

let getRotation =
  Memoize.partialMemoize3((_, _, spread) => {
    let f = () => Math.random() *. 2.0 *. spread;
    let (rx, ry, rz) = (f(), f(), f());
    (rx, ry, rz);
  });

let run = (gl, time) => {
  let geometryType = ConfigVars.geometryType#get();
  let fg = ConfigVars.foregroundColor#get();
  let bg = ConfigVars.backgroundColor#get();
  let width = state.window.width;
  let height = state.window.height;
  let count = ConfigVars.count#get();
  switch (getShaderProgram(gl, fg, bg)) {
  | (uniforms, Some(program)) =>
    Memoize.setMemoizeId(program);
    Document.debug(Document.window, gl);
    let (_, buffers, vao) = getGeometryAndBuffers(gl, program, geometryType);
    WebGL2Util.preRender(gl, width, height);
    Math.globalSeedRandom(ConfigVars.seed#get());
    for (i in 1 to count) {
      let (x, y, z) =
        getPosition(i, ConfigVars.seed#get(), ConfigVars.spread#get());
      let (rx, ry, rz) =
        getRotation(i, ConfigVars.seed#get(), ConfigVars.rotationSpread#get());
      let sz = ConfigVars.size#get();
      let iseed = float_of_int(i);
      renderObj(
        gl,
        program,
        buffers,
        vao,
        (sz, sz, sz),
        (x, y, z),
        (rx, ry, rz),
        width,
        height,
        time *. (1.0 +. iseed /. float_of_int(count)) +. iseed,
        uniforms
      );
    };
  | (_, None) => raise(NoProgram)
  };
};

let rec renderLoop = (startTime, canvas, gl, startIteration) => {
  let t = Date.now() -. startTime;
  run(gl, t /. 1000.0);
  let currentIteration = Document.iteration(Document.window);
  if (currentIteration == startIteration) {
    Document.requestAnimationFrame(() =>
      renderLoop(startTime, canvas, gl, startIteration)
    );
  } else {
    let _ = Document.removeChild(canvas);
    Js.log(
      "exiting render loop "
      ++ string_of_int(startIteration)
      ++ " due to hot reload"
    );
  };
};

let main = (_) => {
  let (canvas, gl) = setupDocument();
  let startTime = Date.now();
  let startIteration = Document.iteration(Document.window);
  renderLoop(startTime, canvas, gl, startIteration);
  () => {
    Js.log("destroying last app generation");
    ConfigUI.destroy();
  };
};
