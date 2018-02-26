open! GLSL;

open GLSLUniforms;

exception Bug(string);

let a_uv = vec2attr("a_uv");

let v_uv = vec2varying("v_uv");

let input = sampler2Duniform("t_input");

let input2 = sampler2Duniform("t_input2");

let heightMap = sampler2Duniform("heightMap");

let uvMul = vec22f(f(2.0) / f(Terrain.getWidth()), f(2.0) / f(Terrain.getHeight()));

let vertexShader =
  body(() => {
    v_uv =@ a_uv;
    let position = vec3var("position");
    position =@ gl_Vertex **. xyz';
    let uv = vec2var("uv");
    uv =@ position **. xy' * f(0.5) * uvMul + f(0.5);
    position **. z' =@ texture(heightMap, uv) **. x';
    position **. xy' *= (f(0.3) * uvMul);
    gl_Position
    =@ u_projectionMatrix
    * u_modelViewMatrix
    * vec4(position |+| f(1.0));
  });

let fragmentShader =
  body(() =>
    gl_FragColor =@ f(1.0) * texture(input, v_uv * f(10.0)) + vec41f(f(0.0))
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

let getUniforms = (texture) => [
  r(u_modelViewMatrix, arg => arg.modelViewMatrix),
  r(u_projectionMatrix, arg => arg.projectionMatrix),
  registeredTiles,
  registeredTiles2,
  registerTextureUniform(heightMap, (_) =>
    switch texture^ {
    | Some(x) => x
    | None => raise(Bug("uninitialized shader texture copy"))
    }
  )
];

let makeProgramSource =
  Memoize.partialMemoize1((texture) => {
    let uniformBlock = getUniforms(texture);
    (uniformBlock, getProgram(uniformBlock, vertexShader, fragmentShader));
  });
