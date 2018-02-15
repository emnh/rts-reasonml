/* One specific shader */
open GLSL;

let a_uv = vec2attr("a_uv");

let v_uv = vec2varying("v_uv");

let position = vec4attr("a_position");

let modelViewMatrix = mat4uniform("modelViewMatrix");

let projectionMatrix = mat4uniform("projectionMatrix");

let u_color1 = vec4uniform("u_color1");

let u_color2 = vec4uniform("u_color2");

let resolution = vec2uniform("u_resolution");

let time = floatuniform("u_time");

let mainVertex =
  gfun(
    Void,
    "main",
    () => {
      open! VertexShader;
      /* gl_Position is a special variable a vertex shader is responsible for setting */
      v_uv =@ a_uv;
      gl_Position
      =@ projectionMatrix
      * modelViewMatrix
      * vec4([position **. XYZ, f(1.0)]);
      finish();
    }
  );

let mainFragment =
  gfun(
    Void,
    "main",
    () => {
      open! FragmentShader;
      let position = vec2var("position");
      position =@ gl_FragCoord **. XY / (resolution **. XY);
      let color = floatvar("color");
      color =@ f(0.0);
      color
      += (
        sin(position **. X * cos(time / f(15.0)) * f(80.0))
        + cos(position **. Y * cos(time / f(15.0)) * f(10.0))
      );
      color
      += (
        sin(position **. Y * sin(time / f(10.0)) * f(40.0))
        + cos(position **. X * sin(time / f(25.0)) * f(40.0))
      );
      color
      += (
        sin(position **. X * sin(time / f(5.0)) * f(10.0))
        + sin(position **. Y * sin(time / f(35.0)) * f(80.0))
      );
      color *= (sin(time / f(10.0)) * f(0.5));
      color += (u_color1 + u_color2);
      gl_FragColor
      =@ vec4([
           vec3([color, color * f(0.5), sin(color + time / f(3.0)) * f(0.75)]),
           f(1.0)
         ]);
      gl_FragColor =@ position **. XYZW;
      finish();
    }
  );

let program = getProgram(mainVertex, mainFragment);

let (vertexShader, fragmentShader) = (
  program.vertexShader,
  program.fragmentShader
);
