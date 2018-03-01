open! GLSL;

open GLSLUniforms;

exception Bug(string);

let u_resolution = vec2uniform("u_resolution");

let input = sampler2Duniform("t_input");

let vertexShader =
  body(() =>
    gl_Position
    =@ u_projectionMatrix
    * u_modelViewMatrix
    * vec4(gl_Vertex **. xyz' |+| f(1.0))
  );

let fragmentShader =
  body(() => {
    gl_FragColor =@ texture(input, gl_FragCoord **. xy' / u_resolution);
    /* XXX: for debug 
      gl_FragColor **. gba' =@ vec33f(f(0.0), f(0.0), f(1.0));
     */
    gl_FragColor **. a' =@ f(1.0);
  });

let scale = f(0.5);

/* Depends on parameters below */
let maxHeight = (f(0.1) + f(0.002) + f(0.0005) + f(0.00025)) * scale;

let uv = vec2arg("uv");

/* For terrain */
let heightMapBody =
  body(() => {
    let value = floatvar("value");
    let uv = vec3(uv |+| u_time / f(20.0));
    value
    =@ ShaderAshima3.snoise(uv * f(1.63))
    * f(0.1)
    + ShaderAshima3.snoise(uv * f(10.0))
    * f(0.002)
    + ShaderAshima3.snoise(uv * f(20.0))
    * f(0.0005)
    + ShaderAshima3.snoise(uv * f(40.0))
    * f(0.00025)
    /*
     + ShaderAshima.snoise(uv * f(40.0))
     * f(0.002)
     * */
    + ShaderAshima3.snoise(uv * f(20.0))
    * f(0.01);
    return(value * f(2.0));
  });

let heightMap = x => fundecl1(floatfun("heightmap"), uv, heightMapBody, x);

/* For water */
let heightMapBody2 =
  body(() => {
    let value = floatvar("value");
    value
    =@ ShaderAshima.snoise(uv * f(1.63))
    * f(0.1)
    + ShaderAshima.snoise(uv * f(10.0))
    * f(0.002)
    + ShaderAshima.snoise(uv * f(20.0))
    * f(0.0005)
    + ShaderAshima.snoise(uv * f(40.0))
    * f(0.00025);
    let rnd = ShaderAshima.snoise(uv * f(1.0));
    ifstmt(rnd < f(0.0), () =>
      value += ShaderAshima.snoise(uv * f(40.0)) * f(0.005)
    );
    return(value * f(2.0));
  });

let heightMap2 = x => fundecl1(floatfun("heightmap"), uv, heightMapBody2, x);

/* TODO: move random shader to another file. shouldn't be in ShaderCopy as it's
 * not a copy shader :p */
let randomFragmentShader = computeNormal =>
  body(() => {
    let uv = vec2var("uv");
    uv =@ gl_FragCoord **. xy' / u_resolution;
    let value = floatvar("value");
    let delta = vec21f(f(1.0)) / u_resolution;
    let eps = delta **. x';
    let hm =
      if (Pervasives.(!)(computeNormal)) {
        heightMap2;
      } else {
        heightMap;
      };
    value =@ hm(uv);
    let p = uv;
    let a =
      hm(vec22f(p **. x' - eps, p **. y'))
      - hm(vec22f(p **. x' + eps, p **. y'));
    let b = f(2.0) * eps;
    let c =
      hm(vec22f(p **. x', p **. y' - eps))
      - hm(vec22f(p **. x', p **. y' + eps));
    let normal = vec3var("normal");
    normal =@ vec33f(a, b, c);
    normal =@ normalize(normal);
    /* Don't compute normals for water here, because they are stored differently there.
     * We have another shader for that.
     * */
    if (Pervasives.(!)(computeNormal)) {
      normal =@ vec31f(f(0.0));
    };
    /* TODO: uniform for max height */
    gl_FragColor
    =@ vec44f(value * scale, normal **. x', normal **. y', normal **. z');
  });

let r = registerUniform;

let getUniforms = texture => [
  r(u_modelViewMatrix, arg => arg.modelViewMatrix),
  r(u_projectionMatrix, arg => arg.projectionMatrix),
  r(u_resolution, arg =>
    [|float_of_int(arg.width), float_of_int(arg.height)|]
  ),
  registerTextureUniform(input, (_) =>
    switch texture^ {
    | Some(x) => x
    | None => raise(Bug("uninitialized shader texture copy"))
    }
  )
];

let getUniforms2 = () => [
  r(u_modelViewMatrix, arg => arg.modelViewMatrix),
  r(u_projectionMatrix, arg => arg.projectionMatrix),
  r(u_resolution, arg => [|float_of_int(arg.width), float_of_int(arg.height)|]),
  r(u_time, arg => [|arg.time|])
];

let makeProgramSource = texture => {
  let uniformBlock = getUniforms(texture);
  (uniformBlock, getProgram(uniformBlock, vertexShader, fragmentShader));
};

let makeRandomProgramSource =
  Memoize.partialMemoize0(() => {
    let uniformBlock = getUniforms2();
    (
      uniformBlock,
      getProgram(uniformBlock, vertexShader, randomFragmentShader(true))
    );
  });

let makeRandomProgramSource2 =
  Memoize.partialMemoize0(() => {
    let uniformBlock = getUniforms2();
    (
      uniformBlock,
      getProgram(uniformBlock, vertexShader, randomFragmentShader(false))
    );
  });
