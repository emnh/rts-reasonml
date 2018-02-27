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

let twist = false;

exception NoGL;

exception No2D;

exception NoProgram;

exception NoTimerQuery;

exception Bug;

let getShaderExampleProgram =
  Memoize.partialMemoize2((fg, bg) => ShaderExample.makeProgramSource(fg, bg));

let getWaterProgram =
  Memoize.partialMemoize1(tref => WaterRenderer.Water.makeProgramSource(tref));

let getWaterNormalProgram =
  Memoize.partialMemoize1(tref =>
    WaterRenderer.Water.makeNormalProgramSource(tref)
  );

let getCopyProgram =
  Memoize.partialMemoize1(t => ShaderCopy.makeProgramSource(t));

let getShaderProgram =
  Memoize.partialMemoize3((gl, uniforms, programSource: GLSL.programT) => {
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

let getGeometryAndBuffers =
  Memoize.partialMemoize3((gl, program, gt) => {
    let geometry =
      switch gt {
      | "Box" => Three.createBoxGeometry()
      | "Sphere" => Three.createSphereGeometry()
      | "Plane" => Three.createPlaneGeometry()
      | "Quad" => Three.createQuadGeometry()
      | _ => Three.createBoxGeometry()
      };
    let buffers = WebGL2Util.createBuffers(gl, geometry);
    let vao = WebGL2Util.createAttributes(gl, program, buffers);
    (geometry, buffers, vao);
  });

let getCamera = (width, height) => {
  let cameraPosition = (
    ConfigVars.cameraX#get(),
    ConfigVars.cameraY#get() *. Terrain.getWidth() /. 2.0,
    ConfigVars.cameraZ#get()
  );
  let cameraRotation = (
    ConfigVars.cameraRotationX#get(),
    ConfigVars.cameraRotationY#get(),
    ConfigVars.cameraRotationZ#get()
  );
  (
    Three.getCamera(width, height, cameraPosition, cameraRotation),
    cameraPosition
  );
};

let renderObj =
    (
      gl,
      program,
      (camera, cameraPosition),
      buffers,
      vao,
      size,
      pos,
      rot,
      width,
      height,
      time,
      uniforms,
      twist,
      measure
    ) => {
  let obj: Three.objectTransformT = Three.getObjectMatrix(pos, size, rot);
  let viewMatrices: Three.viewTransformT =
    Three.getViewMatrices(camera, obj.matrixWorld, twist);
  let (uniformBlock, textures) =
    GLSLUniforms.computeUniformBlock(
      gl,
      time,
      width,
      height,
      cameraPosition,
      viewMatrices.modelViewMatrix,
      viewMatrices.projectionMatrix,
      uniforms
    );
  WebGL2Util.renderObject(
    gl,
    program,
    buffers,
    textures,
    vao,
    uniformBlock,
    measure
  );
};

let seedrandom = Math.localSeedRandom();

let getPosition =
  Memoize.partialMemoize3((_, _, spread) => {
    let f = () => (Math.random() -. 0.5) *. 2.0 *. spread;
    let (rx, ry, rz) = (f(), f(), 0.0);
    (rx, ry, rz);
  });

let getRotation =
  Memoize.partialMemoize3((_, _, spread) => {
    let f = () => Math.random() *. 2.0 *. spread;
    let (rx, ry, rz) = (f(), f(), f());
    (rx, ry, rz);
  });

let getWaterRT =
  Memoize.partialMemoize4((gl, width, height, _) =>
    WebGL2Util.createRenderTarget(gl, width, height)
  );

let runFrameBuffer =
    (
      gl,
      time,
      renderTarget: option(WebGL2Util.renderTargetT),
      programSource,
      geoType,
      measure
    ) => {
  let (uniforms, shaderProgramSource) = programSource;
  let drawToScreen =
    switch renderTarget {
    | Some(_) => false
    | None => true
    };
  let getRenderTarget = () =>
    switch renderTarget {
    | Some(x) => x
    | None => raise(Bug)
    };
  let width =
    if (drawToScreen) {
      state.window.width;
    } else {
      getRenderTarget().width;
    };
  let height =
    if (drawToScreen) {
      state.window.height;
    } else {
      getRenderTarget().height;
    };
  switch (getShaderProgram(gl, uniforms, shaderProgramSource)) {
  | (uniforms, Some(program)) =>
    Memoize.setMemoizeId(program);
    /*
     let geometry = Three.createQuadGeometry();
     let buffers = WebGL2Util.createBuffers(gl, geometry);
     let vao = WebGL2Util.createAttributes(gl, program, buffers);
     */
    let (_, buffers, vao) = getGeometryAndBuffers(gl, program, geoType);
    WebGL2Util.preRender(gl, width, height);
    Math.globalSeedRandom(ConfigVars.seed#get());
    let (x, y, z) = (0.0, 0.0, (-1.0));
    let (rx, ry, rz) = (0.0, 0.0, 0.0);
    let sz = 2.0;
    let cameraPosition = (0.0, 0.0, 0.0);
    let rf = () =>
      renderObj(
        gl,
        program,
        (
          Three.getCamera(width, height, cameraPosition, (0.0, 0.0, 0.0)),
          cameraPosition
        ),
        buffers,
        vao,
        (sz, sz, sz),
        (x, y, z),
        (rx, ry, rz),
        width,
        height,
        time,
        uniforms,
        twist,
        measure
      );
    if (drawToScreen) {
      rf();
    } else {
      WebGL2Util.renderToTarget(gl, getRenderTarget(), rf);
    };
  | (_, None) => raise(NoProgram)
  };
};

let run = (gl, time, uAndProgram, measure) => {
  let geometryType = ConfigVars.geometryType#get();
  let width = state.window.width;
  let height = state.window.height;
  let count = ConfigVars.count#get();
  let (uniforms, shaderProgramSource) = uAndProgram;
  switch (getShaderProgram(gl, uniforms, shaderProgramSource)) {
  | (uniforms, Some(program)) =>
    Memoize.setMemoizeId(program);
    Document.debug(Document.window, gl);
    Document.debug(Document.window, uniforms);
    let (_, buffers, vao) = getGeometryAndBuffers(gl, program, geometryType);
    WebGL2Util.preRender(gl, width, height);
    Math.globalSeedRandom(ConfigVars.seed#get());
    for (i in 1 to count) {
      let (bx, by, bz) = (
        ConfigVars.objectX#get(),
        ConfigVars.objectY#get(),
        ConfigVars.objectZ#get()
      );
      let (x, y, z) =
        getPosition(i, ConfigVars.seed#get(), ConfigVars.spread#get());
      let (x, y, z) = (bx +. x, by +. y, bz +. z);
      let (rx, ry, rz) =
        getRotation(i, ConfigVars.seed#get(), ConfigVars.rotationSpread#get());
      let rspeed = ConfigVars.rotationSpeed#get();
      let (rx, ry, rz) = (
        rspeed *. Math.sin(time) *. 2.0 *. Math.pi +. rx,
        rspeed *. Math.sin(0.35 *. time) *. 2.0 *. Math.pi +. ry,
        rspeed *. Math.sin(0.73 *. time) *. 2.0 *. Math.pi +. rz
      );
      let sz = ConfigVars.size#get();
      let iseed = float_of_int(i);
      let (camera, cameraPos) = getCamera(width, height);
      Three.cameraLookAt3(camera, x, y, z);
      renderObj(
        gl,
        program,
        (camera, cameraPos),
        buffers,
        vao,
        (sz, sz, sz),
        (x, y, z),
        (rx, ry, rz),
        width,
        height,
        time *. (1.0 +. iseed /. float_of_int(count)) +. iseed,
        uniforms,
        twist,
        measure
      );
    };
  | (_, None) => raise(NoProgram)
  };
};

let targetIndex = ref(0);

let textureRef = ref(None);

let causticsRef = ref(None);

let heightMapRef = ref(None);

let terrainRenderRef = ref(None);

let getMeasure =
  Memoize.partialMemoize3((gl, ext, _) => {
    let query = WebGL2.createQuery(gl);
    let measure = f => {
      WebGL2.beginQuery(gl, WebGL2.getTIME_ELAPSED_EXT(ext), query);
      f();
      WebGL2.endQuery(gl, WebGL2.getTIME_ELAPSED_EXT(ext));
    };
    let lastValue = ref(0);
    let readMeasure = () => {
      let available =
        WebGL2.getQueryParameterBool(
          gl,
          query,
          WebGL2.getQUERY_RESULT_AVAILABLE(gl)
        );
      let disjoint =
        WebGL2.getParameterBool(gl, WebGL2.getGPU_DISJOINT_EXT(ext));
      if (available && ! disjoint) {
        /* See how much time the rendering of the object took in nanoseconds. */
        let nanoseconds =
          WebGL2.getQueryParameterInt(gl, query, WebGL2.getQUERY_RESULT(gl))
          / 1000;
        lastValue := nanoseconds;
        Some(nanoseconds);
      } else {
        None;
      };
    };
    measure(() => ());
    (measure, readMeasure, () => lastValue^);
  });

let reportElement =
  Memoize.partialMemoize0(() => {
    let elem = Document.createElement("div");
    let _ = Document.appendChild(elem);
    let style = Document.getStyle(elem);
    Document.setPosition(style, "fixed");
    Document.setTop(style, "0px");
    Document.setLeft(style, "100px");
    elem;
  });

let doMeasure2 = (gl, queryExt, name) => {
  let (defaultMeasure, _, _) = getMeasure(gl, queryExt, "Default");
  let (measure, readMeasure, getLast) = getMeasure(gl, queryExt, name);
  let rep = reportElement();
  let measure =
    switch (readMeasure()) {
    | Some(nanoseconds) =>
      let p = Document.createElement("p");
      Document.setInnerHTML(p, name ++ ":" ++ string_of_int(nanoseconds));
      let _ = Document.appendChild2(rep, p);
      measure;
    | None =>
      let p = Document.createElement("p");
      Document.setInnerHTML(p, name ++ ":" ++ string_of_int(getLast()));
      let _ = Document.appendChild2(rep, p);
      defaultMeasure;
    };
  measure;
};

/*
 let doMeasure = (_, _, _, f) => f();
 */
let doMeasure = doMeasure2;

let runPipeline = (gl, queryExt, time) => {
  let sz = 256;
  let width = sz;
  let height = sz;
  let renderTarget1 = getWaterRT(gl, width, height, "wrt1");
  let renderTarget2 = getWaterRT(gl, width, height, "wrt2");
  let csz = 1024;
  let renderTargetCaustics = getWaterRT(gl, csz, csz, "wrt3");
  let renderTargetTerrain = getWaterRT(gl, 512, 512, "wrt4");
  let heightMapRT = getWaterRT(gl, 1024, 1024, "wrt5");
  let switchTargets = () => {
    targetIndex := (targetIndex^ + 1) mod 2;
    switch targetIndex^ {
    | 0 => renderTarget1
    | _ => renderTarget2
    };
  };
  let quad = "Quad";
  let renderTarget = switchTargets();
  let rep = reportElement();
  Document.setInnerHTML(rep, "");
  /* Compute height map with normals */
  switch heightMapRef^ {
  | Some(_) => ()
  | None =>
    runFrameBuffer(
      gl,
      time,
      Some(heightMapRT),
      ShaderCopy.makeRandomProgramSource(1.0),
      quad,
      doMeasure2(gl, queryExt, "HeightMap")
    );
    ();
  };
  heightMapRef := Some(heightMapRT.texture);
  /* Compute initial wave, without normals */
  switch textureRef^ {
  | Some(_) => ()
  | None =>
    runFrameBuffer(
      gl,
      time,
      Some(renderTarget),
      ShaderCopy.makeRandomProgramSource2(-1.0),
      quad,
      doMeasure(gl, queryExt, "Initial wave")
    );
    ();
  };
  textureRef := Some(renderTarget.texture);
  /* Compute waves */
  let renderTarget = switchTargets();
  runFrameBuffer(
    gl,
    time,
    Some(renderTarget),
    getWaterProgram(textureRef),
    quad,
    doMeasure(gl, queryExt, "Waves")
  );
  textureRef := Some(renderTarget.texture);
  /* Compute waves 2 */
  let renderTarget = switchTargets();
  runFrameBuffer(
    gl,
    time,
    Some(renderTarget),
    getWaterProgram(textureRef),
    quad,
    doMeasure(gl, queryExt, "Waves2")
  );
  textureRef := Some(renderTarget.texture);
  /* Compute normals */
  let renderTarget = switchTargets();
  runFrameBuffer(
    gl,
    time,
    Some(renderTarget),
    getWaterNormalProgram(textureRef),
    quad,
    doMeasure(gl, queryExt, "Normals")
  );
  /* Next time use previous output as input */
  textureRef := Some(renderTarget.texture);
  /* Compute caustics */
  runFrameBuffer(
    gl,
    time,
    Some(renderTargetCaustics),
    WaterRenderer.Renderer.makeCausticsProgramSource(textureRef),
    "Plane",
    doMeasure(gl, queryExt, "Caustics")
  );
  causticsRef := Some(renderTargetCaustics.texture);
  /* Render terrain */
  runFrameBuffer(
    gl,
    time,
    Some(renderTargetTerrain),
    ShaderTerrain.makeProgramSource(heightMapRef),
    "Plane",
    doMeasure(gl, queryExt, "Render terrain")
  );
  /* Render water */
  terrainRenderRef := Some(renderTargetTerrain.texture);
  run(
    gl,
    time,
    WaterRenderer.Renderer.makeProgramSource(
      textureRef,
      causticsRef,
      terrainRenderRef,
      heightMapRef
    ),
    doMeasure(gl, queryExt, "Render water")
  );
  /* Copy to screen for debug */
  /* runFrameBuffer(gl, time, None, getCopyProgram(textureRef)); */
  /* runFrameBuffer(gl, time, None, getCopyProgram(causticsRef), quad, doMeasure(gl, queryExt, "Copy")); */
};

let runDemo = (gl, time) => {
  let fg = ConfigVars.foregroundColor#get();
  let bg = ConfigVars.backgroundColor#get();
  /* run(gl, time, getWaterRendererProgram(ref(None))); */
  run(gl, time, getShaderExampleProgram(fg, bg));
};

let rec renderLoop = (queryExt, stats, startTime, canvas, gl, startIteration) => {
  let t = Date.now() -. startTime;
  Stats.beginStats(stats);
  runPipeline(gl, queryExt, t /. 1000.0);
  Stats.endStats(stats);
  /*
   runDemo(gl, t /. 1000.0);
   */
  let currentIteration = Document.iteration(Document.window);
  if (currentIteration == startIteration) {
    Document.requestAnimationFrame(()
      => renderLoop(queryExt, stats, startTime, canvas, gl, startIteration));
      /*
       Document.setTimeout(
         () => renderLoop(queryExt, stats, startTime, canvas, gl, startIteration),
         0
       );
       */
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
  let stats = Stats.createStats();
  let statsdom = Stats.dom(stats);
  let _ = Document.appendChild(statsdom);
  let style = Document.getStyle(statsdom);
  Document.setMargin(style, "10px");
  let ext = WebGL2.getExtension(gl, "EXT_disjoint_timer_query_webgl2");
  let oext = Js.Nullable.to_opt(ext);
  let queryExt =
    switch oext {
    | Some(ext) => ext
    | None => raise(NoTimerQuery)
    };
  renderLoop(queryExt, stats, startTime, canvas, gl, startIteration);
  () => {
    Js.log("destroying last app generation");
    ConfigUI.destroy();
  };
};
