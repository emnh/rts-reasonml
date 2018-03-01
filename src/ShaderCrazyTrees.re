open! GLSL;

open GLSLUniforms;

exception Bug(string);

let objectId = floatuniform("objectId");

let tree = sampler2Duniform("t_tree");

let heightMap = sampler2Duniform("heightMap");

let a_uv = vec2attr("a_uv");

let v_uv = vec2varying("v_uv");

let v_position_uv = vec2varying("v_position");

let rcolor = vec3varying("v_rcolor");

let vertexShader =
  body(() => {
    v_uv =@ f(1.0) - a_uv;
    let vertexId = floatvar("vertexId");
    let vertexId2 = floatvar("vertexId2");
    vertexId =@ int2float(idiv(gl_VertexId, i(4))) / f(100000.0);
    vertexId2
    =@ int2float(idiv(gl_VertexId, i(4)))
    / f(100000.0)
    + f(0.1342352);
    /*
     vertexId =@ int2float(gl_VertexId) - fmod(int2float(gl_VertexId), f(4.0));
     */
    let position = vec3var("position");
    let rand1 = vertexId * f(0.239543) + f(0.243254);
    let rand2 = fmod(rand1, f(1.2345));
    let rand3 = vertexId * f(7.823579) + f(0.432423);
    let rand4 = fmod(rand3, f(0.23404));
    let rand5 = vertexId * f(3.7812314);
    let rand6 = fmod(rand5, f(0.254654));
    let rand7 = vertexId2 * f(7.823579) + f(0.432423);
    let rand8 = fmod(rand3, f(0.23404));
    let rand9 = vertexId2 * f(3.7812314);
    let rand10 = fmod(rand5, f(0.254654));
    let quadMul =
      f(0.01)
      + f(0.02)
      * (ShaderLib.rand(vec22f(rand1, rand2)) - f(0.5) + f(1.0));
    let quadMul = quadMul * f(float_of_int(Terrain.getTileWidth()));
    let treeHeightMul = f(2.0);
    position =@ gl_Vertex **. xyz' * quadMul;
    let uv = vec2var("uv");
    let c = f(2.0);
    /* Trees are not square */
    position **. y' *= treeHeightMul;
    let xoffset = floatvar("xoffset");
    xoffset =@ c * (ShaderLib.rand(vec22f(rand3, rand4)) - f(0.5));
    let zoffset = floatvar("zoffset");
    zoffset =@ c * (ShaderLib.rand(vec22f(rand5, rand6)) - f(0.5));
    let xoffset2 = floatvar("xoffset2");
    xoffset2 =@ c * (ShaderLib.rand(vec22f(rand7, rand8)) - f(0.5));
    let zoffset2 = floatvar("zoffset2");
    zoffset2 =@ c * (ShaderLib.rand(vec22f(rand9, rand10)) - f(0.5));
    rcolor
    =@ vec33f(
         ShaderLib.rand(vec22f(xoffset, zoffset)),
         ShaderLib.rand(vec22f(zoffset2, xoffset2)),
         ShaderLib.rand(vec22f(xoffset + xoffset2, zoffset + zoffset2))
       );
    let t = floatvar("t");
    t
    =@ fmod(
         u_time
         / f(5.0)
         * (f(0.5) + ShaderLib.rand(vec22f(xoffset2, zoffset2)))
         + ShaderLib.rand(vec22f(xoffset, zoffset)),
         f(2.0)
       )
    /*
     sin(u_time / f(5.0)) + f(1.0)
     */
    / f(2.0);
    xoffset =@ mix(f(0.0), xoffset2, t);
    zoffset =@ mix(f(0.0), zoffset2, t);
    let ixy = vec2var("ixy");
    ixy =@ Terrain.getIXY(objectId);
    position **. x' += xoffset;
    position **. z' += zoffset;
    /*
     let xoffset2 = (xoffset + ixy **. x') * (Terrain.getHMMul() **. x');
     let zoffset2 = (zoffset + ixy **. y') * (Terrain.getHMMul() **. y');
     */
    uv =@ vec22f(xoffset, zoffset) * f(0.5) + f(0.5);
    uv =@ (uv + ixy) * Terrain.getHMMul();
    /* Trees are blowing in the wind */
    ifstmt(position **. y' > f(0.0), () =>
      position **. x' += f(0.0025) * ShaderAshima.snoise(uv + u_time * f(0.5))
    );
    let hn = vec4var("heightNormal");
    hn =@ texture(heightMap, uv);
    let quadMinY = f(1.0) * quadMul * treeHeightMul;
    let yoffset = floatvar("yoffset");
    let t2 = t - f(0.5);
    let t2 = f(0.75) - f(3.0) * t2 * t2;
    yoffset =@ hn **. x' * Terrain.heightMultiplier + t2;
    /* TODO: get water height from somewhere else. lake map. */
    /* Don't put trees in water or too close to edge of map */
    ifelsestmt(
      yoffset > f(0.05) && abs(xoffset) < f(0.9) && abs(zoffset) < f(1.9),
      () => position **. y' += (yoffset + quadMinY),
      () => position **. y' =@ f(1000.0)
    );
    v_position_uv =@ uv;
    position **. xz' += Terrain.getTiledOffset(objectId);
    gl_Position
    =@ u_projectionMatrix
    * u_modelViewMatrix
    * vec4(position |+| f(1.0));
  });

let fragmentShader =
  body(() => {
    let color = vec4var("color");
    color =@ texture(tree, v_uv) * f(1.5);
    let gray = dot(color **. rgb', vec33f(f(0.299), f(0.587), f(0.114)));
    /* ifstmt(color **. r' + color **. g' + color **. b' > f(0.3), () => */
    ifelsestmt(
      color **. a' < f(0.5),
      () => discard(),
      () => color **. a' *= f(1.5)
    );
    /*
     color **. a' =@ color **. r' + color **. g' + color **. b';
     */
    /*
     color **. rgb' *= (color **. a');
     */
    color
    *= (f(0.75) + f(0.25) * ShaderTerrain.getClouds(v_position_uv, u_time));
    color **. rgb' =@ gray * rcolor * f(2.0);
    gl_FragColor =@ color;
  });

let r = registerUniform;

let treeTexture = ref(None);

let registeredTrees =
  registerTextureUniform(
    tree,
    arg => {
      let retval =
        switch treeTexture^ {
        | Some(texture) => texture
        | None => getNewTexture(arg.gl, "resources/tree.png")
        };
      treeTexture := Some(retval);
      retval;
    }
  );

let getUniforms = heightMapRef => [
  r(u_modelViewMatrix, arg => arg.modelViewMatrix),
  r(u_projectionMatrix, arg => arg.projectionMatrix),
  r(u_time, arg => [|arg.time|]),
  r(objectId, arg => [|float_of_int(arg.objectId)|]),
  registeredTrees,
  registerTextureUniform(heightMap, (_) =>
    switch heightMapRef^ {
    | Some(x) => x
    | None => raise(Bug("uninitialized shader texture copy"))
    }
  )
];

let makeProgramSource =
  Memoize.partialMemoize1(heightMapRef => {
    let uniformBlock = getUniforms(heightMapRef);
    (uniformBlock, getProgram(uniformBlock, vertexShader, fragmentShader));
  });
