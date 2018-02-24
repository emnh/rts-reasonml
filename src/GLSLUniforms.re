type uniformInputT = {
  gl: WebGL2.glT,
  time: float,
  tick: float,
  width: int,
  height: int,
  modelViewMatrix: array(float),
  projectionMatrix: array(float)
};

/*
 type uniformTextureT = {
    wrapS: WebGL2.glT => WebGL2.clampT,
    wrapT: WebGL2.glT => WebGL2.clampT,
   name: string,
   texture: WebGL2.glT => WebGL2.textureT
 };
    */
type uniformFunctionT =
  | UniformFloatArray(uniformInputT => array(float))
  | UniformTexture(string, uniformInputT => WebGL2.textureT);

type uniformBlockT = list((GLSL.rT, uniformInputT => array(float)));

let registerUniform = (a, b) => (GLSL.untyped(a), UniformFloatArray(b));

let registerTextureUniform = (a, b) => {
  let a = GLSL.untyped(a);
  let name =
    switch a {
    | GLSL.RVar((_, _, name)) => name
    | _ => raise(GLSL.GLSLTypeError("bad uniform"))
    };
  (a, UniformTexture(name, b));
};

let cachedEmptyTexture = ref(None);

let emptyTexture = gl => {
  let retval =
    switch cachedEmptyTexture^ {
    | Some(x) => x
    | None => WebGL2.createTexture(gl)
    };
  retval;
};

let setupTexture = (gl, _) => {
  let t2d = WebGL2.getTEXTURE_2D(gl);
  WebGL2.texParameteri(
    gl,
    t2d,
    WebGL2.getTEXTURE_WRAP_S(gl),
    WebGL2.getCLAMP_TO_EDGE(gl)
  );
  WebGL2.texParameteri(
    gl,
    t2d,
    WebGL2.getTEXTURE_WRAP_T(gl),
    WebGL2.getCLAMP_TO_EDGE(gl)
  );
  WebGL2.texParameteri(
    gl,
    t2d,
    WebGL2.getTEXTURE_MIN_FILTER(gl),
    WebGL2.getNEAREST(gl)
  );
  WebGL2.texParameteri(
    gl,
    t2d,
    WebGL2.getTEXTURE_MAG_FILTER(gl),
    WebGL2.getNEAREST(gl)
  );
};

/* TODO: Preallocate and refill array */
let computeUniformBlock =
    (gl, time, width, height, modelViewMatrix, projectionMatrix, uniforms) => {
  let uniformArg = {
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
        | UniformFloatArray(f2) => f2(uniformArg)
        | _ => [||]
        },
      uniforms
    );
  let uniformBlock = Float32Array.create(Array.concat(l));
  let textures =
    List.fold_right(
      ((_, f), r) =>
        switch f {
        | UniformTexture(name, f2) => [(name, f2(uniformArg)), ...r]
        | _ => r
        },
      uniforms,
      []
    );
  (uniformBlock, textures);
};
