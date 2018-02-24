/* One specific shader */
open! GLSL;

open GLSLUniforms;

let a_uv = vec2attr("a_uv");

let v_uv = vec2varying("v_uv");

let a_position = vec4attr("a_position");

let u_modelViewMatrix = mat4uniform("modelViewMatrix");

let u_projectionMatrix = mat4uniform("projectionMatrix");

let u_color1 = vec4uniform("u_color1");

let u_color2 = vec4uniform("u_color2");

let u_resolution = vec2uniform("u_resolution");

let u_time = floatuniform("u_time");

let fmtColor = color => {
  let (r, g, b, _) = color;
  let c = 256.0;
  [|float_of_int(r) /. c, float_of_int(g) /. c, float_of_int(b) /. c, 1.0|];
};

let r = registerUniform;

let getUniforms = (fg, bg) => [
  r(u_modelViewMatrix, arg => arg.modelViewMatrix),
  r(u_projectionMatrix, arg => arg.projectionMatrix),
  r(u_color1, (_) => fmtColor(fg)),
  r(u_color2, (_) => fmtColor(bg)),
  r(
    u_resolution,
    arg => [|float_of_int(arg.width), float_of_int(arg.height)|]
  ),
  r(u_time, arg => [|arg.time|])
];

let mainVertex =
  body(() => {
    /* gl_Position is a special variable a vertex shader is responsible for setting */
    v_uv =@ a_uv;
    let position = vec3var("position");
    position =@ a_position **. xyz';
    position
    **. z'
    =@ ShaderAshima.snoise(a_position **. xy' + f(2.0) * u_time)
    * f(0.2);
    gl_Position
    =@ u_projectionMatrix
    * u_modelViewMatrix
    * vec4(position **. xyz' |+| f(1.0));
  });

let mainFragment =
  body(() => {
    let position = vec2var("position");
    /*
     position =@ gl_FragCoord **. XY / (resolution **. XY);
     */
    let resolution = vec2var("resolution");
    resolution =@ u_resolution;
    position =@ v_uv;
    let color = floatvar("color");
    color =@ f(0.0);
    color
    += (
      sin(position **. x' * cos(u_time / f(15.0)) * f(80.0))
      + cos(position **. y' * cos(u_time / f(15.0)) * f(10.0))
    );
    color
    += (
      sin(position **. y' * sin(u_time / f(10.0)) * f(40.0))
      + cos(position **. x' * sin(u_time / f(25.0)) * f(40.0))
    );
    color
    += (
      sin(position **. x' * sin(u_time / f(5.0)) * f(10.0))
      + sin(position **. y' * sin(u_time / f(35.0)) * f(80.0))
    );
    color *= (sin(u_time / f(10.0)) * f(0.5));
    outColor
    =@ vec4(
         vec33f(color, color * f(0.5), sin(color + u_time / f(3.0)) * f(0.75))
         |+| f(1.0)
       );
    outColor += (u_color1 + u_color2);
    outColor **. r' += ShaderAshima.snoise(v_uv);
    outColor **. g' += ShaderAshima.snoise(v_uv + f(123.234));
    outColor **. b' += ShaderAshima.snoise(v_uv - f(323.234));
  });

let makeProgramSource = (fg, bg) => {
  let uniformBlock = getUniforms(fg, bg);
  (uniformBlock, getProgram(uniformBlock, mainVertex, mainFragment));
};
