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

let getShaderProgram =
  Memoize.partialMemoize(
    3,
    [|1, 2|],
    (gl, fg, bg) => {
      let (uniforms, programSource) = ShaderExample.makeProgramSource(fg, bg);
      let vertexShaderSource = programSource.vertexShader;
      let fragmentShaderSource = programSource.fragmentShader;
      Js.log("Vertex shader:");
      Js.log(MyString.lineNumbers(vertexShaderSource));
      let vertexShader =
        WebGL2.createShader(
          gl,
          WebGL2.getVERTEX_SHADER(gl),
          vertexShaderSource
        );
      Js.log("Fragment shader:");
      Js.log(MyString.lineNumbers(fragmentShaderSource));
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
    | Some(gl) => gl
    | None =>
      showError("No WebGL2!");
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

let geof =
  Memoize.memoize(1, gt =>
    (
      switch gt {
      | "Box" => Three.createBoxGeometry
      | "Sphere" => Three.createSphereGeometry
      | "Plane" => Three.createPlaneGeometry
      | _ => Three.createBoxGeometry
      }
    )
      ()
  );

let renderObj = (gl, program, buffers, pos, rot, width, height, time, uniforms) => {
  let sz = 6.0;
  let (rx, ry, rz) = rot;
  let obj: Three.objectTransformT =
    Three.getObjectMatrix(
      pos,
      (sz, sz, sz),
      (
        Math.sin(time) *. 2.0 *. Math.pi +. rx,
        Math.sin(0.35 *. time) *. 2.0 *. Math.pi +. ry,
        Math.sin(0.73 *. time) *. 2.0 *. Math.pi +. rz
      )
    );
  let viewMatrices: Three.viewTransformT =
    Three.getViewMatrices(obj.matrixWorld, width, height);
  let uniformBlock =
    WebGL2.computeUniformBlock(
      time,
      width,
      height,
      viewMatrices.modelViewMatrix,
      viewMatrices.projectionMatrix,
      uniforms
    );
  WebGL2.renderObject(gl, program, buffers, uniformBlock);
};

let getPosition = Memoize.memoize(1, (_) => {
  let (rx, ry, rz) = (Math.random(), Math.random(), -20.0);
  (rx, ry, rz);
});

let getRotation = Memoize.memoize(1, (_) => {
  let (rx, ry, rz) = (Math.random(), Math.random(), Math.random());
  (rx, ry, rz);
});

let run = (gl, time) => {
  let geometryType = ConfigVars.geometryType#get();
  let fg = ConfigVars.foregroundColor#get();
  let bg = ConfigVars.backgroundColor#get();
  let width = state.window.width;
  let height = state.window.height;
  let geometry = geof(geometryType);
  switch (getShaderProgram(gl, fg, bg)) {
  | (uniforms, Some(program)) =>
    WebGL2.preRender(gl, width, height);
    let buffers = WebGL2.createBuffers(gl, geometry);
    for (i in 0 to 100) {
      let (x, y, z) = getPosition(i);
      let (rx, ry, rz) = getRotation(i);
      renderObj(
        gl,
        program,
        buffers,
        (x, y, z),
        (rx, ry, rz),
        width,
        height,
        time,
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
