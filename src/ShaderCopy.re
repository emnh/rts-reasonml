open! GLSL;

open GLSLUniforms;

exception Bug(string);

let u_resolution = vec2uniform("u_resolution");

let input = sampler2Duniform("t_input");

let computeNormal = floatuniform("u_computeNormal");

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
    /* XXX: for debug */
    gl_FragColor **. a' =@ f(1.0);
  });

let scale = f(0.5);

/* Depends on parameters below */
let maxHeight = (f(0.1) + f(0.002) + f(0.0005) + f(0.00025)) * scale;

let uv = vec2arg("uv");

let heightMapBody =
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
    /*
    + ShaderAshima.snoise(uv * f(40.0))
    * f(0.002);
    + ShaderAshima.snoise(uv * f(20.0))
    * f(0.01);
    */
    return(value * f(2.0));
  });

let heightMap = x => fundecl1(floatfun("heightmap"), uv, heightMapBody, x);

/* TODO: move random shader to another file. shouldn't be in ShaderCopy as it's
 * not a copy shader :p */
let randomFragmentShader = computeNormal =>
  body(() => {
    let uv = vec2var("uv");
    uv =@ gl_FragCoord **. xy' / u_resolution;
    let value = floatvar("value");
    let delta = vec21f(f(1.0)) / u_resolution;
    let eps = delta **. x';
    value =@ heightMap(uv);
    let hm = heightMap;
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
    ifstmt(computeNormal < f(0.0), () => normal =@ vec31f(f(0.0)));
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

let getUniforms2 = cn => [
  r(u_modelViewMatrix, arg => arg.modelViewMatrix),
  r(u_projectionMatrix, arg => arg.projectionMatrix),
  r(u_resolution, arg =>
    [|float_of_int(arg.width), float_of_int(arg.height)|]
  ),
  r(computeNormal, (_) => [|cn|])
];

let makeProgramSource = texture => {
  let uniformBlock = getUniforms(texture);
  (uniformBlock, getProgram(uniformBlock, vertexShader, fragmentShader));
};

let makeRandomProgramSource =
  Memoize.partialMemoize1(cn => {
    let uniformBlock = getUniforms2(cn);
    (
      uniformBlock,
      getProgram(uniformBlock, vertexShader, randomFragmentShader(f(cn)))
    );
  });

let makeRandomProgramSource2 =
  Memoize.partialMemoize1(cn => {
    let uniformBlock = getUniforms2(cn);
    (
      uniformBlock,
      getProgram(uniformBlock, vertexShader, randomFragmentShader(f(cn)))
    );
  });
