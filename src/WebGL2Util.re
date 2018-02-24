open WebGL2;

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

/* TODO: Preallocate and refill array */
let computeUniformBlock =
    (gl, time, width, height, modelViewMatrix, projectionMatrix, uniforms) => {
  let uniformArg: GLSLUniforms.uniformInputT = {
    gl,
    time,
    tick: 0.0,
    width,
    height,
    modelViewMatrix,
    projectionMatrix
  };
  let l =
    List.map(
      ((_, f)) =>
        switch f {
        | GLSLUniforms.UniformFloatArray(f2) => f2(uniformArg)
        | _ => [||]
        },
      uniforms
    );
  let uniformBlock = Float32Array.create(Array.concat(l));
  uniformBlock;
};

let preRender = (gl, width, height) => {
  viewport(gl, 0, 0, width, height);
  enable(gl, getDEPTH_TEST(gl));
  clearColor(gl, 0, 0, 0, 0);
  clear(gl, getCOLOR_BUFFER_BIT(gl));
};

type glBuffersT = {
  positionBuffer: bufferT,
  uvBuffer: bufferT,
  indexBuffer: bufferT,
  offset: int,
  count: int
};

let createBuffers = (gl, geometry: Three.geometryBuffersT) => {
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
  bufferDataInt16(gl, getELEMENT_ARRAY_BUFFER(gl), index, getSTATIC_DRAW(gl));
  let offset = 0;
  let count = Int16Array.length(index);
  {positionBuffer, uvBuffer, indexBuffer, offset, count};
};

let createAttributes = (gl, program, buffers) => {
  let vao = createVertexArray(gl);
  bindVertexArray(gl, vao);
  let positionAttributeLocation = getAttribLocation(gl, program, "a_position");
  bindBuffer(gl, getARRAY_BUFFER(gl), buffers.positionBuffer);
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
  bindBuffer(gl, getARRAY_BUFFER(gl), buffers.uvBuffer);
  let uvAttributeLocation = getAttribLocation(gl, program, "a_uv");
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
  vao;
};

let getUniformBufferAndLocation =
  Memoize.partialMemoize2((gl, program) => {
    let uniformPerSceneBuffer = createBuffer(gl);
    let uniformPerSceneLocation =
      getUniformBlockIndex(gl, program, "u_PerScene");
    (uniformPerSceneBuffer, uniformPerSceneLocation);
  });

let renderObject = (gl, program, buffers, vao, uniformBlock) => {
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
  /* Bind buffers */
  bindBuffer(gl, getARRAY_BUFFER(gl), buffers.positionBuffer);
  bindBuffer(gl, getELEMENT_ARRAY_BUFFER(gl), buffers.indexBuffer);
  /* Render */
  drawElements(
    gl,
    getTRIANGLES(gl),
    buffers.count,
    getUNSIGNED_SHORT(gl),
    buffers.offset
  );
};
