open! GLSL;

open GLSLUniforms;

exception Bug(string);

let a_uv = vec2attr("a_uv");

let v_uv = vec2varying("v_uv");

let input = sampler2Duniform("t_input");

let input2 = sampler2Duniform("t_input2");

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
    return(value * f(2.0));
  });

let heightMap = x => fundecl1(floatfun("heightmap"), uv, heightMapBody, x);

let vertexShader =
  body(() => {
    v_uv =@ a_uv;
    let position = vec3var("position");
    position =@ gl_Vertex **. xyz';
    let uv = vec2var("uv");
    uv =@ position **. xy';
    position **. z' =@ heightMap(uv);
    position **. xy' *= f(0.3);
    gl_Position
    =@ u_projectionMatrix
    * u_modelViewMatrix
    * vec4(position |+| f(1.0));
  });

let fragmentShader =
  body(() =>
    gl_FragColor =@ f(1.0) * texture(input, v_uv * f(2.0)) + vec41f(f(0.0))
  );

let r = registerUniform;

let tilesTexture = ref(None);

let registeredTiles =
  registerTextureUniform(
    input,
    arg => {
      let retval =
        switch tilesTexture^ {
        | Some(texture) => texture
        | None => getNewTexture(arg.gl, "/resources/grass.jpg")
        };
      tilesTexture := Some(retval);
      retval;
    }
  );

let tiles2Texture = ref(None);

let registeredTiles2 =
  registerTextureUniform(
    input2,
    arg => {
      let retval =
        switch tiles2Texture^ {
        | Some(texture) => texture
        | None => getNewTexture(arg.gl, "/resources/tiles.jpg")
        };
      tiles2Texture := Some(retval);
      retval;
    }
  );

let getUniforms = () => [
  r(u_modelViewMatrix, arg => arg.modelViewMatrix),
  r(u_projectionMatrix, arg => arg.projectionMatrix),
  registeredTiles,
  registeredTiles2
];

let makeProgramSource =
  Memoize.partialMemoize0(() => {
    let uniformBlock = getUniforms();
    (uniformBlock, getProgram(uniformBlock, vertexShader, fragmentShader));
  });
