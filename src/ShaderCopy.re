open! GLSL;

open GLSLUniforms;

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
  body(() =>
    gl_FragColor =@ texture(input, gl_FragCoord **. xy' / u_resolution)
  );

let r = registerUniform;

let getUniforms = texture => [
  r(u_modelViewMatrix, arg => arg.modelViewMatrix),
  r(u_projectionMatrix, arg => arg.projectionMatrix),
  r(u_resolution, arg =>
    [|float_of_int(arg.width), float_of_int(arg.height)|]
  ),
  registerTextureUniform(input, (_) => texture)
];

let makeProgramSource = texture => {
  let uniformBlock = getUniforms(texture);
  (uniformBlock, getProgram(uniformBlock, vertexShader, fragmentShader));
};
