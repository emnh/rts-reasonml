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
   texture: WebGL2.glT => WebGL2.textureT
 };
 */
type uniformFunctionT =
  | UniformFloatArray(uniformInputT => array(float))
  | UniformTexture(uniformInputT => WebGL2.textureT);

type uniformBlockT = list((GLSL.rT, uniformInputT => array(float)));

let registerUniform = (a, b) => (GLSL.untyped(a), UniformFloatArray(b));

let registerTextureUniform = (a, b) => (GLSL.untyped(a), UniformTexture(b));

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
