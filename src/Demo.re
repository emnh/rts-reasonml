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

exception NoDerivativesExtension;

exception NoElementIndexExtension;

exception Bug;

let getShaderExampleProgram =
  Memoize.partialMemoize2((fg, bg) => ShaderExample.makeProgramSource(fg, bg));

let getWaterProgram =
  Memoize.partialMemoize2((tref, href) =>
    WaterRenderer.Water.makeProgramSource(tref, href)
  );

let getWaterNormalProgram =
  Memoize.partialMemoize2((tref, href) =>
    WaterRenderer.Water.makeNormalProgramSource(tref, href)
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

let showError = msg => {
  let body = Document.body;
  Document.setInnerHTML(body, "<h1 style='color: red;'>" ++ msg ++ "</h1>");
};

let setupDocument = () => {
  Document.setMargin(Document.getStyle(Document.body), "0px");
  Document.setOverflow(Document.getStyle(Document.body), "hidden");
  let canvas = Document.createElement("canvas");
  let _ = Document.appendChild(canvas);
  Js.log(Document.userAgent);
  let (gl, version) = {
    let (ngl, tryversion) =
      if (MyString.indexOf(Document.userAgent, "Chromium/62") != (-1)
          || MyString.indexOf(Document.userAgent, "Windows") == (-1)) {
        (Js.Nullable.toOption(WebGL2.getContext(canvas, "webgl")), 1);
      } else {
        (Js.Nullable.toOption(WebGL2.getContext(canvas, "webgl2")), 2);
      };
    switch ngl {
    | Some(gl) =>
      Memoize.setMemoizeId(gl);
      (gl, tryversion);
    | None =>
      switch (Js.Nullable.toOption(WebGL2.getContext(canvas, "webgl"))) {
      | Some(gl) =>
        Memoize.setMemoizeId(gl);
        (gl, 1);
      | None =>
        showError("No WebGL2!");
        raise(NoGL);
      }
    };
  };
  let showLink = () => {
    let elem = Document.createElement("div");
    let _ = Document.appendChild(elem);
    let style = Document.getStyle(elem);
    Document.setPosition(style, "fixed");
    Document.setTop(style, "80px");
    Document.setLeft(style, "0px");
    Document.setInnerHTML(
      elem,
      "<a href=\"https://github.com/emnh/rts-reasonml\">GitHub</<a>"
    );
  };
  let showVersion = () => {
    let elem = Document.createElement("div");
    let _ = Document.appendChild(elem);
    let style = Document.getStyle(elem);
    Document.setPosition(style, "fixed");
    Document.setTop(style, "100px");
    Document.setLeft(style, "0px");
    Document.setInnerHTML(elem, "Using WebGL ver: " ++ string_of_int(version));
  };
  showLink();
  showVersion();
  WebGL2.setMY_VERSION(gl, version);
  GLSL.webGLVersion := version;
  let setCanvasSize = (_) => {
    let width = Document.getWidth(Document.window);
    let height = Document.getHeight(Document.window);
    /*
    let width = 3840;
    let height = 2160;
    let width = 1920;
    let height = 1080;
    */
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
  Memoize.partialMemoize2((gl, gt) => {
    let geometry =
      switch gt {
      | "Box" => Three.createBoxGeometry()
      | "Sphere" => Three.createSphereGeometry()
      | "Plane" => Three.createPlaneGeometry()
      | "BigPlane" => Three.createBigPlaneGeometry()
      | "Quad" => Three.createQuadGeometry()
      | "Trees" => Three.createTreesGeometry()
      | _ => Three.createBoxGeometry()
      };
    let buffers = WebGL2Util.createBuffers(gl, geometry);
    /*
     let vao = WebGL2Util.createAttributes(gl, program, buffers);
     */
    (geometry, buffers, ());
  });

let buildTriplet = Memoize.partialMemoize3((x, y, z) => {
  (x, y, z);
});

let getCamera = (width, height) => {
  let cameraPosition = buildTriplet(
    ConfigVars.cameraX#get(),
    ConfigVars.cameraY#get() *. float_of_int(Terrain.getTileWidth()),
    ConfigVars.cameraZ#get() *. float_of_int(Terrain.getTileHeight())
  );
  let cameraRotation = buildTriplet(
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
      tick,
      uniforms,
      twist,
      measure,
      objectId
    ) => {
  let obj: Three.objectTransformT = Three.getObjectMatrix(pos, size, rot);
  let viewMatrices: Three.viewTransformT =
    Three.getViewMatrices(camera, obj.matrixWorld, twist);
  let (uniforms, uniformBlock, textures) =
    GLSLUniforms.computeUniformBlock(
      gl,
      time,
      tick,
      width,
      height,
      cameraPosition,
      Three.getElements(obj.matrixWorld),
      viewMatrices.modelViewMatrix,
      viewMatrices.projectionMatrix,
      objectId,
      uniforms
    );
  WebGL2Util.renderObject(
    gl,
    program,
    buffers,
    textures,
    vao,
    uniforms,
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
      tick,
      renderTarget: option(WebGL2Util.renderTargetT),
      programSource,
      geoType,
      measure,
      count
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
    let (_, buffers, vao) = getGeometryAndBuffers(gl, geoType);
    WebGL2Util.preRender(gl, width, height);
    /*
    Math.globalSeedRandom(ConfigVars.seed#get());
    */
    let (x, y, z) = (0.0, 0.0, (-1.0));
    let (rx, ry, rz) = (0.0, 0.0, 0.0);
    let sz = 2.0;
    let cameraPosition = buildTriplet(0.0, 0.0, 0.0);
    let rf = i =>
      renderObj(
        gl,
        program,
        (
          Three.getCamera(width, height, cameraPosition, buildTriplet(0.0, 0.0, 0.0)),
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
        tick,
        uniforms,
        twist,
        measure,
        i
      );
    for (i in 0 to count - 1) {
      if (drawToScreen) {
        rf(i);
      } else {
        WebGL2Util.renderToTarget(gl, getRenderTarget(), () => rf(i));
      };
    };
  | (_, None) => raise(NoProgram)
  };
};

let run = (gl, time, tick, uAndProgram, measure, geometryType, count) => {
  /*
   let geometryType = ConfigVars.geometryType#get();
   */
  let width = state.window.width;
  let height = state.window.height;
  /* ConfigVars.count#get(); */
  let (uniforms, shaderProgramSource) = uAndProgram;
  switch (getShaderProgram(gl, uniforms, shaderProgramSource)) {
  | (uniforms, Some(program)) =>
    Memoize.setMemoizeId(program);
    Document.debug(Document.window, gl);
    Document.debug(Document.window, uniforms);
    /*
    Math.globalSeedRandom(ConfigVars.seed#get());
    */
    /*
     let irows = int_of_float(Math.ceil(Math.sqrt(float_of_int(count))));
     */
    /*
     let irows = Terrain.getTileWidth();
     */
    let (_, buffers, vao) = getGeometryAndBuffers(gl, geometryType);
    for (i in 1 to count) {
      let ix = 0; /* (i - 1) mod irows; */
      let iy = 0; /* (i - 1) / irows; */
      let (cx, cy, cz) = (
        ConfigVars.objectX#get(),
        ConfigVars.objectY#get(),
        ConfigVars.objectZ#get()
      );
      let (bx, by, bz) = (
        cx +. float_of_int(ix) *. Terrain.getWidth(),
        cy,
        cz +. float_of_int(iy) *. Terrain.getHeight()
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
      Three.cameraLookAt3(camera, 0.0, 0.0, 0.0);
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
        tick,
        uniforms,
        twist,
        measure,
        i - 1
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
    /* Initial zero measurement, to start the read loop */
    measure(() => ());
    (measure, readMeasure, () => lastValue^);
  });

let reportElement =
  Memoize.partialMemoize0(() => {
    let elem = Document.createElement("div");
    let _ = Document.appendChild(elem);
    let style = Document.getStyle(elem);
    Document.setPosition(style, "fixed");
    Document.setTop(style, "140px");
    Document.setLeft(style, "0");
    elem;
  });

let doMeasure2 = (gl, queryExt, name) =>
  switch queryExt {
  | Some(queryExt) =>
    let (defaultMeasure, _, _) = getMeasure(gl, queryExt, "Default");
    let (measure, readMeasure, getLast) = getMeasure(gl, queryExt, name);
    let rep = reportElement();
    let measure =
      switch (readMeasure()) {
      | Some(nanoseconds) =>
        let p = Document.createElement("p");
        Document.setInnerHTML(
          p,
          name ++ ":" ++ string_of_int(nanoseconds) ++ "ms"
        );
        let _ = Document.appendChild2(rep, p);
        measure;
      | None =>
        let p = Document.createElement("p");
        Document.setInnerHTML(
          p,
          name ++ ":" ++ string_of_int(getLast()) ++ "ms"
        );
        let _ = Document.appendChild2(rep, p);
        defaultMeasure;
      };
    measure;
  | None => (f => f())
  };

/*
 let doMeasure = (_, _, _, f) => f();
 */
let doMeasure = doMeasure2;

let runPipeline = (gl, queryExt, time, tick) => {
  /*
  let sz = 128 * Terrain.getTileWidth();
  let sz =
    if (sz < 256) {
      256;
    } else {
      sz;
    };
    */
  let sz2 = 256;
  let width = sz2;
  let height = sz2;
  let renderTarget1 = getWaterRT(gl, width, height, "wrt1");
  let renderTarget2 = getWaterRT(gl, width, height, "wrt2");
  let csz = 1024;
  let renderTargetCaustics = getWaterRT(gl, csz, csz, "wrt3");
  let renderTargetTerrain = getWaterRT(gl, 1024, 1024, "wrt4");
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
      tick,
      Some(heightMapRT),
      ShaderCopy.makeRandomProgramSource(),
      quad,
      doMeasure2(gl, queryExt, "HeightMap"),
      1
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
      tick,
      Some(renderTarget),
      ShaderCopy.makeRandomProgramSource2(),
      quad,
      doMeasure(gl, queryExt, "Initial wave"),
      1
    );
    ();
  };
  textureRef := Some(renderTarget.texture);
  /* Compute waves */
  let renderTarget = switchTargets();
  runFrameBuffer(
    gl,
    time,
    tick,
    Some(renderTarget),
    getWaterProgram(textureRef, heightMapRef),
    quad,
    doMeasure(gl, queryExt, "Waves"),
    1
  );
  textureRef := Some(renderTarget.texture);
  /* Compute waves 2 */
  let renderTarget = switchTargets();
  runFrameBuffer(
    gl,
    time,
    tick,
    Some(renderTarget),
    getWaterProgram(textureRef, heightMapRef),
    quad,
    doMeasure(gl, queryExt, "Waves2"),
    1
  );
  textureRef := Some(renderTarget.texture);
  /* Compute normals */
  let renderTarget = switchTargets();
  runFrameBuffer(
    gl,
    time,
    tick,
    Some(renderTarget),
    getWaterNormalProgram(textureRef, heightMapRef),
    quad,
    doMeasure(gl, queryExt, "Normals"),
    1
  );
  /* Next time use previous output as input */
  textureRef := Some(renderTarget.texture);
  /* Compute caustics */
  runFrameBuffer(
    gl,
    time,
    tick,
    Some(renderTargetCaustics),
    WaterRenderer.Renderer.makeCausticsProgramSource(textureRef),
    "Plane",
    doMeasure(gl, queryExt, "Caustics"),
    1
  );
  causticsRef := Some(renderTargetCaustics.texture);
  /* Render terrain */
  runFrameBuffer(
    gl,
    time,
    tick,
    Some(renderTargetTerrain),
    ShaderTerrain.makeProgramSource(heightMapRef),
    "BigPlane",
    doMeasure(gl, queryExt, "Render terrain"),
    1
  );
  /* Render water */
  let count = Terrain.getTileWidth() * Terrain.getTileHeight();
  terrainRenderRef := Some(renderTargetTerrain.texture);
  WebGL2Util.preRender(gl, state.window.width, state.window.height);
  let geometryType = ConfigVars.geometryType#get();
  run(
    gl,
    time,
    tick,
    WaterRenderer.Renderer.makeProgramSource(
      textureRef,
      causticsRef,
      terrainRenderRef,
      heightMapRef
    ),
    doMeasure(gl, queryExt, "Render water"),
    geometryType,
    count
  );
  /* Render trees */
  /*
   WebGL2.blendFunc(gl, WebGL2.getSRC_ALPHA(gl), WebGL2.getONE(gl));
   */
  WebGL2.blendFunc(
    gl,
    WebGL2.getSRC_ALPHA(gl),
    WebGL2.getONE_MINUS_SRC_ALPHA(gl)
  );
  WebGL2.enable(gl, WebGL2.getBLEND(gl));
  /*
   WebGL2.disable(gl, WebGL2.getDEPTH_TEST(gl));
   */
  /*
  run(
    gl,
    time,
    tick,
    /*
     ShaderTrees.makeProgramSource(textureRef),
     */
    ShaderTrees.makeProgramSource(heightMapRef),
    doMeasure(gl, queryExt, "Render trees"),
    "Trees",
    count * 1
  );
  */
  WebGL2.enable(gl, WebGL2.getDEPTH_TEST(gl));
  WebGL2.disable(gl, WebGL2.getBLEND(gl));
  /* Copy to screen for debug */
  /*
    runFrameBuffer(
      gl,
      time,
      None,
      getCopyProgram(terrainRenderRef),
      quad,
      doMeasure(gl, queryExt, "Copy"),
      1
    );
   */
  /* runFrameBuffer(gl, time, None, getCopyProgram(textureRef)); */
  /* runFrameBuffer(gl, time, None, getCopyProgram(causticsRef), quad, doMeasure(gl, queryExt, "Copy"), 1); */
};

let runDemo = (gl, time, tick) => {
  let fg = ConfigVars.foregroundColor#get();
  let bg = ConfigVars.backgroundColor#get();
  /* run(gl, time, getWaterRendererProgram(ref(None))); */
  run(gl, time, tick, getShaderExampleProgram(fg, bg));
};

let oldTime = ref(0.0);

type capturerT;

let capturer: capturerT = [%bs.raw "window.capturer"];

[@bs.send] external startCapture : capturerT => unit = "start";

[@bs.send]
external capture : (capturerT, Document.element) => unit = "capture";

[@bs.send] external save : capturerT => unit = "save";

let rec renderLoop =
        (reset, queryExt, stats, startTime, canvas, gl, startIteration, count) => {
  let t = Date.now() -. startTime;
  let tick = (t -. oldTime^) *. 60.0 /. 1000.0;
  let tick = tick < 0.1 ? 0.1 : tick;
  let tick = tick > 5.0 ? 5.0 : tick;
  oldTime := t;
  Stats.beginStats(stats);
  runPipeline(gl, queryExt, t /. 1000.0, tick);
  Stats.endStats(stats);
  /*
   runDemo(gl, t /. 1000.0);
   */
  let currentIteration = Document.iteration(Document.window);
  if (currentIteration == startIteration) {
    /* reset(); */
    Document.requestAnimationFrame(() =>
      renderLoop(reset, queryExt, stats, startTime, canvas, gl, startIteration, count + 1)
    );
    /*
    capture(capturer, canvas);
    if (count > 0 && count mod 60 == 0) {
      save(capturer);
    }
    */
  } else {
    /*
     Document.setTimeout(
       () => renderLoop(queryExt, stats, startTime, canvas, gl, startIteration),
       0
     );
     */
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
  let oext = Js.Nullable.toOption(ext);
  let queryExt = oext;
  let reset = GLReset.createReset(gl);
  if (WebGL2.getMY_VERSION(gl) == 1) {
    let ext2 = WebGL2.getExtension(gl, "OES_standard_derivatives");
    if (ext2 == Js.Nullable.null) {
      showError("No OES_standard_derivatives extension to WebGL1!");
      raise(NoDerivativesExtension);
    };
    let ext3 = WebGL2.getExtension(gl, "OES_element_index_uint");
    if (ext3 == Js.Nullable.null) {
      showError("No OES_element_index_uint extension to WebGL1!");
      raise(NoElementIndexExtension);
    };
  };
  renderLoop(reset, queryExt, stats, startTime, canvas, gl, startIteration, 0);
  /*
  startCapture(capturer);
  */
  () => {
    Js.log("destroying last app generation");
    ConfigUI.destroy();
  };
};
