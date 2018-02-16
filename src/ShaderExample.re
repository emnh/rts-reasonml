/* One specific shader */
open GLSL;

let a_uv = vec2attr("a_uv");

let v_uv = vec2varying("v_uv");

let position = vec4attr("a_position");

let modelViewMatrix = mat4uniform("modelViewMatrix");

let projectionMatrix = mat4uniform("projectionMatrix");

let u_color1 = vec4uniform("u_color1");

let u_color2 = vec4uniform("u_color2");

let u_resolution = vec2uniform("u_resolution");

let u_time = floatuniform("u_time");

let fmtColor = color => {
  let (r, g, b, _) = color;
  let c = 256.0;
  [|float_of_int(r) /. c, float_of_int(g) /. c, float_of_int(b) /. c, 1.0|];
};

let getUniforms = (fg, bg) => [
  (modelViewMatrix, arg => arg.modelViewMatrix),
  (projectionMatrix, arg => arg.projectionMatrix),
  (u_color1, (_) => fmtColor(fg)),
  (u_color2, (_) => fmtColor(bg)),
  (u_resolution, arg => [|float_of_int(arg.width), float_of_int(arg.height)|]),
  (u_time, arg => [|arg.time|])
];

let mainVertex = {
  ast: {
    open! VertexShader;
    /* gl_Position is a special variable a vertex shader is responsible for setting */
    v_uv =@ a_uv;
    gl_Position
    =@ projectionMatrix
    * modelViewMatrix
    * vec4([position **. XYZ, f(1.0)]);
    finish();
  }
};

let mainFragment = {
  ast: {
    open! FragmentShader;
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
      sin(position **. X * cos(u_time / f(15.0)) * f(80.0))
      + cos(position **. Y * cos(u_time / f(15.0)) * f(10.0))
    );
    color
    += (
      sin(position **. Y * sin(u_time / f(10.0)) * f(40.0))
      + cos(position **. X * sin(u_time / f(25.0)) * f(40.0))
    );
    color
    += (
      sin(position **. X * sin(u_time / f(5.0)) * f(10.0))
      + sin(position **. Y * sin(u_time / f(35.0)) * f(80.0))
    );
    color *= (sin(u_time / f(10.0)) * f(0.5));
    outColor
    =@ vec4([
         vec3([color, color * f(0.5), sin(color + u_time / f(3.0)) * f(0.75)]),
         f(1.0)
       ]);
    outColor += (u_color1 + u_color2);
    finish();
  }
};

let makeProgramSource = (fg, bg) => {
  let uniformBlock = getUniforms(fg, bg);
  (uniformBlock, getProgram(uniformBlock, mainVertex, mainFragment));
};
