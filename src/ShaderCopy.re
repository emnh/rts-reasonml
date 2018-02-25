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
    /* XXX: for debug */
    gl_FragColor **. a' =@ f(1.0);
  });

let randomFragmentShader =
  body(() => {
    let uv = vec2var("uv");
    uv =@ gl_FragCoord **. xy' / u_resolution * f(10.0);
    /* TODO: uniform for max height */
    gl_FragColor =@ vec44f(ShaderAshima.snoise(uv) * f(0.002), f(0.0), f(0.0), f(0.0));
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
  r(u_resolution, arg =>
    [|float_of_int(arg.width), float_of_int(arg.height)|]
  ),
];

let makeProgramSource = texture => {
  let uniformBlock = getUniforms(texture);
  (uniformBlock, getProgram(uniformBlock, vertexShader, fragmentShader));
};

let makeRandomProgramSource = () => {
  let uniformBlock = getUniforms2();
  (uniformBlock, getProgram(uniformBlock, vertexShader, randomFragmentShader));
};
