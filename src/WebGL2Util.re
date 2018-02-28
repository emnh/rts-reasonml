open WebGL2;

exception WebGL2Exception(string);

exception CacheNotWorking;

let createShader = (gl, stype, source) => {
  let shader = createShader(gl, stype);
  shaderSource(gl, shader, source);
  compileShader(gl, shader);
  let success = getShaderParameter(gl, shader, getCOMPILE_STATUS(gl));
  if (Js.to_bool(success)) {
    Some(shader);
  } else {
    Js.log(getShaderInfoLog(gl, shader));
    deleteShader(gl, shader);
    None;
  };
};

let createProgram = (gl, vertexShader, fragmentShader) => {
  let program = createProgram(gl);
  attachShader(gl, program, vertexShader);
  attachShader(gl, program, fragmentShader);
  linkProgram(gl, program);
  let success = getProgramParameter(gl, program, getLINK_STATUS(gl));
  if (Js.to_bool(success)) {
    Some(program);
  } else {
    Js.log(getProgramInfoLog(gl, program));
    deleteProgram(gl, program);
    None;
  };
};

let preRender = (gl, width, height) => {
  viewport(gl, 0, 0, width, height);
  enable(gl, getDEPTH_TEST(gl));
  clearColor(gl, 0.9, 0.9, 0.9, 1.0);
  clear(gl, getCOLOR_BUFFER_BIT(gl) lor getDEPTH_BUFFER_BIT(gl));
};

type glBuffersT = {
  positionBuffer: bufferT,
  uvBuffer: bufferT,
  indexBuffer: bufferT,
  offset: int,
  count: int
};

let cbCounter = ref(0);

let createBuffers = (gl, geometry: Three.geometryBuffersT) => {
  cbCounter := cbCounter^ + 1;
  if (cbCounter^ > 10) {
    raise(CacheNotWorking);
  };
  Js.log("createBuffers");
  let positions = geometry.position;
  let index = geometry.index;
  let positionBuffer = createBuffer(gl);
  bindBuffer(gl, getARRAY_BUFFER(gl), positionBuffer);
  bufferData(gl, getARRAY_BUFFER(gl), positions, getSTATIC_DRAW(gl));
  let uvBuffer = createBuffer(gl);
  bindBuffer(gl, getARRAY_BUFFER(gl), uvBuffer);
  bufferData(gl, getARRAY_BUFFER(gl), geometry.uv, getSTATIC_DRAW(gl));
  let indexBuffer = createBuffer(gl);
  bindBuffer(gl, getELEMENT_ARRAY_BUFFER(gl), indexBuffer);
  bufferDataInt32(gl, getELEMENT_ARRAY_BUFFER(gl), index, getSTATIC_DRAW(gl));
  let offset = 0;
  let count = Int32Array.length(index);
  {positionBuffer, uvBuffer, indexBuffer, offset, count};
};

let createAttributes = (gl, program, buffers) => {
  let vao = createVertexArray(gl);
  bindVertexArray(gl, vao);
  let positionAttributeLocation = getAttribLocation(gl, program, "a_position");
  bindBuffer(gl, getARRAY_BUFFER(gl), buffers.positionBuffer);
  if (positionAttributeLocation != (-1)) {
    enableVertexAttribArray(gl, positionAttributeLocation);
    let size = 3;
    let normalize = Js.Boolean.to_js_boolean(false);
    let stride = 0;
    let offset = 0;
    vertexAttribPointer(
      gl,
      positionAttributeLocation,
      size,
      getFLOAT(gl),
      normalize,
      stride,
      offset
    );
  } else {
    ();
      /* Js.log("warning: unused a_position"); */
  };
  bindBuffer(gl, getARRAY_BUFFER(gl), buffers.uvBuffer);
  let uvAttributeLocation = getAttribLocation(gl, program, "a_uv");
  if (uvAttributeLocation != (-1)) {
    enableVertexAttribArray(gl, uvAttributeLocation);
    let size = 2;
    let normalize = Js.Boolean.to_js_boolean(false);
    let stride = 0;
    let offset = 0;
    vertexAttribPointer(
      gl,
      uvAttributeLocation,
      size,
      getFLOAT(gl),
      normalize,
      stride,
      offset
    );
  } else {
    ();
      /* Js.log("warning: unused a_uv"); */
  };
  vao;
};

let getUniformBufferAndLocation =
  Memoize.partialMemoize2((gl, program) => {
    let uniformPerSceneBuffer = createBuffer(gl);
    let uniformPerSceneLocation =
      getUniformBlockIndex(gl, program, "u_PerScene");
    (uniformPerSceneBuffer, uniformPerSceneLocation);
  });

let renderObject = (gl, program, buffers, textures, vao, uniformBlock, measure) => {
  useProgram(gl, program);
  bindVertexArray(gl, vao);
  /* Upload uniforms */
  let (uniformPerSceneBuffer, uniformPerSceneLocation) =
    getUniformBufferAndLocation(gl, program);
  /*
   Js.log((uniformPerSceneBuffer, uniformPerSceneLocation));
   */
  let uniformBlockBindingIndex = 0;
  uniformBlockBinding(
    gl,
    program,
    uniformPerSceneLocation,
    uniformBlockBindingIndex
  );
  bindBuffer(gl, getUNIFORM_BUFFER(gl), uniformPerSceneBuffer);
  bufferData(gl, getUNIFORM_BUFFER(gl), uniformBlock, getDYNAMIC_DRAW(gl));
  bindBuffer(gl, getUNIFORM_BUFFER(gl), Js.Nullable.null);
  bindBufferBase(
    gl,
    getUNIFORM_BUFFER(gl),
    uniformBlockBindingIndex,
    uniformPerSceneBuffer
  );
  /* Bind textures */
  let textureIndices = [
    getTEXTURE0(gl),
    getTEXTURE1(gl),
    getTEXTURE2(gl),
    getTEXTURE3(gl),
    getTEXTURE4(gl),
    getTEXTURE5(gl),
    getTEXTURE6(gl),
    getTEXTURE7(gl),
    getTEXTURE8(gl),
    getTEXTURE9(gl),
    getTEXTURE10(gl),
    getTEXTURE11(gl),
    getTEXTURE12(gl),
    getTEXTURE13(gl),
    getTEXTURE14(gl),
    getTEXTURE15(gl)
  ];
  List.iteri(
    (index, (name, texture)) => {
      let uniformLocation = getUniformLocation(gl, program, name);
      uniform1i(gl, uniformLocation, index);
      activeTexture(gl, List.nth(textureIndices, index));
      bindTexture(gl, getTEXTURE_2D(gl), texture);
    },
    textures
  );
  /* Bind buffers */
  bindBuffer(gl, getARRAY_BUFFER(gl), buffers.positionBuffer);
  bindBuffer(gl, getELEMENT_ARRAY_BUFFER(gl), buffers.indexBuffer);
  /* Render */
  measure(() =>
    drawElements(
      gl,
      getTRIANGLES(gl),
      buffers.count,
      getUNSIGNED_INT(gl),
      buffers.offset
    )
  );
};

type renderTargetT = {
  width: int,
  height: int,
  framebuffer: framebufferT,
  texture: textureT
};

let createRenderTarget = (gl, width, height) => {
  /* TODO: memoize? */
  let a = WebGL2.getExtension(gl, "OES_texture_float_linear");
  if (a == Js.Nullable.null) {
    raise(WebGL2Exception("missing extension OES_texture_float_linear"));
  };
  let b = WebGL2.getExtension(gl, "EXT_color_buffer_float");
  if (b == Js.Nullable.null) {
    raise(WebGL2Exception("missing extension EXT_color_buffer_float"));
  };
  /* create to render to */
  let targetTextureWidth = width;
  let targetTextureHeight = height;
  let targetTexture = createTexture(gl);
  bindTexture(gl, getTEXTURE_2D(gl), targetTexture);
  /* define size and format of level 0 */
  let level = 0;
  let internalFormat = getRGBA32F(gl);
  let border = 0;
  let format = getRGBA(gl);
  let ttype = getFLOAT(gl);
  let data = Js.Nullable.null;
  texImage2DdataFloat(
    gl,
    getTEXTURE_2D(gl),
    level,
    internalFormat,
    targetTextureWidth,
    targetTextureHeight,
    border,
    format,
    ttype,
    data
  );
  /* set the filtering so we don't need mips */
  texParameteri(
    gl,
    getTEXTURE_2D(gl),
    getTEXTURE_MAG_FILTER(gl),
    getLINEAR(gl)
  );
  texParameteri(
    gl,
    getTEXTURE_2D(gl),
    getTEXTURE_MIN_FILTER(gl),
    getLINEAR(gl)
  );
  texParameteri(
    gl,
    getTEXTURE_2D(gl),
    getTEXTURE_WRAP_S(gl),
    getCLAMP_TO_EDGE(gl)
  );
  texParameteri(
    gl,
    getTEXTURE_2D(gl),
    getTEXTURE_WRAP_T(gl),
    getCLAMP_TO_EDGE(gl)
  );
  /* Create and bind the framebuffer */
  let fb = createFramebuffer(gl);
  bindFramebuffer(gl, getFRAMEBUFFER(gl), Js.Nullable.from_opt(Some(fb)));
  /* Attach the texture as the first color attachment */
  let attachmentPoint = getCOLOR_ATTACHMENT0(gl);
  framebufferTexture2D(
    gl,
    getFRAMEBUFFER(gl),
    attachmentPoint,
    getTEXTURE_2D(gl),
    targetTexture,
    level
  );
  /*
     Js.log(checkFramebufferStatus(gl, getFRAMEBUFFER(gl)));
   */
  bindFramebuffer(gl, getFRAMEBUFFER(gl), Js.Nullable.null);
  {
    framebuffer: fb,
    texture: targetTexture,
    width: targetTextureWidth,
    height: targetTextureHeight
  };
};

let renderToTarget = (gl, renderTarget, renderFunction) => {
  /* render to our targetTexture by binding the framebuffer */
  bindFramebuffer(
    gl,
    getFRAMEBUFFER(gl),
    Js.Nullable.from_opt(Some(renderTarget.framebuffer))
  );
  /* Tell WebGL how to convert from clip space to pixels */
  viewport(gl, 0, 0, renderTarget.width, renderTarget.height);
  /* Clear the canvas AND the depth buffer. */
  clearColor(gl, 0.0, 0.0, 1.0, 1.0); /* clear to blue */
  clear(gl, getCOLOR_BUFFER_BIT(gl) lor getDEPTH_BUFFER_BIT(gl));
  renderFunction();
  bindFramebuffer(gl, getFRAMEBUFFER(gl), Js.Nullable.null);
};
