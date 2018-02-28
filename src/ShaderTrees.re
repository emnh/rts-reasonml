open! GLSL;

open GLSLUniforms;

exception Bug(string);

let objectId = floatuniform("objectId");

let tree = sampler2Duniform("t_tree");

let heightMap = sampler2Duniform("heightMap");

let a_uv = vec2attr("a_uv");

let v_uv = vec2varying("v_uv");

let vertexShader =
  body(() => {
    v_uv =@ f(1.0) - a_uv;
    let position = vec3var("position");
    let quadMul = f(0.08);
    let treeHeightMul = f(2.0);
    position =@ gl_Vertex **. xyz' * quadMul;
    let uv = vec2var("uv");
    let c = f(2.0);
    /* Trees are not square */
    position **. y' *= treeHeightMul;
    let objectId =
      objectId + int2float(gl_VertexId) - fmod(int2float(gl_VertexId), f(4.0));
    let xoffset = floatvar("xoffset");
    xoffset
    =@ c
    * (ShaderLib.rand(vec22f(objectId, objectId + f(1.43243))) - f(0.5));
    let objectId = fmod(objectId, f(1.2345));
    let zoffset = floatvar("zoffset");
    zoffset
    =@ c
    * (ShaderLib.rand(vec22f(objectId, objectId + f(3.7812314))) - f(0.5));
    position **. x' += xoffset;
    position **. z' += zoffset;
    uv =@ vec22f(xoffset, zoffset) * f(0.5) + f(0.5);
    let hn = vec4var("heightNormal");
    hn =@ texture(heightMap, uv);
    let quadMinY = f(1.0) * quadMul * treeHeightMul;
    let yoffset = floatvar("yoffset");
    yoffset =@ (hn **. x' * Terrain.heightMultiplier + quadMinY);
    /* TODO: get water height from somewhere else. lake map. */
    ifelsestmt(yoffset > f(0.2), () => {
      position **. y' += yoffset;
    }, () => {
      position **. y' =@ f(1000.0);
    });
    gl_Position
    =@ u_projectionMatrix
    * u_modelViewMatrix
    * vec4(position |+| f(1.0));
  });

let fragmentShader =
  body(() => {
    let color = vec4var("color");
    color =@ texture(tree, v_uv) * f(1.5);
    /* let gray = dot(color **. rgb', vec33f(f(0.299), f(0.587), f(0.114))); */
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
        | None => getNewTexture(arg.gl, "/resources/tree.png")
        };
      treeTexture := Some(retval);
      retval;
    }
  );

let getUniforms = heightMapRef => [
  r(u_modelViewMatrix, arg => arg.modelViewMatrix),
  r(u_projectionMatrix, arg => arg.projectionMatrix),
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
