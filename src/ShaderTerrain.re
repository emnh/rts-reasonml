open! GLSL;

open GLSLUniforms;

exception Bug(string);

let a_uv = vec2attr("a_uv");

let v_uv = vec2varying("v_uv");

let input = sampler2Duniform("t_input");

let input2 = sampler2Duniform("t_input2");

let heightMap = sampler2Duniform("heightMap");

let uvMul =
  vec22f(f(2.0) / f(Terrain.getWidth()), f(2.0) / f(Terrain.getHeight()));

let vNormal = vec3varying("v_normal");

let light = vec3uniform("light");

let vertexShader =
  body(() => {
    v_uv =@ a_uv;
    let position = vec3var("position");
    position =@ gl_Vertex **. xyz';
    let uv = vec2var("uv");
    uv =@ position **. xy' * f(0.5) * uvMul + f(0.5);
    let hn = vec4var("heightNormal");
    hn =@ texture(heightMap, uv);
    let normal = vec3var("normal");
    normal =@ hn **. yzw';
    vNormal =@ normal;
    position **. z' =@ hn **. x';
    position **. xy' *= (f(0.3) * uvMul);
    gl_Position
    =@ u_projectionMatrix
    * u_modelViewMatrix
    * vec4(position |+| f(1.0));
  });

let getClouds = (uv, u_time) =>
  ShaderAshima.snoise(uv * f(5.0) + ShaderAshima.snoise(uv + u_time / f(10.0)))
  * f(0.5)
  + f(1.2);

let fragmentShader =
  body(() => {
    let color = vec3var("color");
    let diffuse = vec3var("diffuse");
    let uv = vec2var("uv");
    uv =@ v_uv;
    color =@ texture(input, uv * f(10.0)) **. rgb';
    let gray = dot(color **. rgb', vec33f(f(0.299), f(0.587), f(0.114)));
    color *= (pow(gray, ShaderLib.rand(uv)) * f(2.0));
    diffuse
    =@ color
    * clamp(dot(vNormal, light) * getClouds(uv, u_time), f(0.0), f(1.5));
    /*
     uv =@ vec22f(f(1.0) - v_uv**.x', v_uv**.y');
     color =@ texture(input, uv * f(10.0)) **. rgb';
     clouds
     =@ ShaderAshima.snoise(
          uv * f(5.0) + ShaderAshima.snoise(uv + u_time / f(10.0))
        )
     * f(0.5)
     + f(1.2);
     diffuse += color * clamp(dot(vNormal, light) * clouds, f(0.0), f(1.5));
     diffuse /= f(2.0);
     */
    gl_FragColor =@ vec4(diffuse |+| f(1.0));
  });

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

let getUniforms = texture => [
  r(u_modelViewMatrix, arg => arg.modelViewMatrix),
  r(u_projectionMatrix, arg => arg.projectionMatrix),
  r(u_time, arg => [|arg.time|]),
  r(
    light,
    (_) => {
      let (x, y, z) = (
        ConfigVars.lightX#get(),
        ConfigVars.lightY#get(),
        ConfigVars.lightZ#get()
      );
      let (x, y, z) = ShaderLib.normalize(x, y, z);
      [|x, y, z|];
    }
  ),
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
  Memoize.partialMemoize1(texture => {
    let uniformBlock = getUniforms(texture);
    (uniformBlock, getProgram(uniformBlock, vertexShader, fragmentShader));
  });
