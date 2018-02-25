type materialT;

type geometryT;

type boxT = geometryT;

type sphereT = geometryT;

type planeT = geometryT;

type meshT;

type attributesT;

type arrayBufferT;

type cameraT;

type matrix4T;

type vector3T;

type positionT = vector3T;

[@bs.new] [@bs.module "three"]
external createBoxBufferGeometry : (float, float, float, int, int, int) => boxT =
  "BoxBufferGeometry";

[@bs.new] [@bs.module "three"]
external createSphereBufferGeometry : (float, int, int) => sphereT =
  "SphereBufferGeometry";

[@bs.new] [@bs.module "three"]
external createPlaneBufferGeometry : (float, float, int, int) => planeT =
  "PlaneBufferGeometry";

[@bs.new] [@bs.module "three"]
external createBasicMaterial : unit => materialT = "MeshBasicMaterial";

[@bs.new] [@bs.module "three"]
external createMesh : (geometryT, materialT) => meshT = "Mesh";

[@bs.new] [@bs.module "three"]
external createPerspectiveCamera : (float, float, float, float) => cameraT =
  "PerspectiveCamera";

[@bs.get] external getAttributes : boxT => attributesT = "attributes";

[@bs.get] external getPosition : attributesT => arrayBufferT = "position";

[@bs.get] external getUV : attributesT => arrayBufferT = "uv";

[@bs.get] external getFloat32Array : arrayBufferT => Float32Array.t = "array";

[@bs.get] external getInt32Array : arrayBufferT => Int32Array.t = "array";

[@bs.get] external getIndex : boxT => arrayBufferT = "index";

[@bs.get] external getElements : matrix4T => array(float) = "elements";

[@bs.set] external setElements : (matrix4T, array(float)) => unit = "elements";

[@bs.get] external getMeshMatrixWorld : meshT => matrix4T = "matrixWorld";

[@bs.get]
external getCameraMatrixWorldInverse : cameraT => matrix4T =
  "matrixWorldInverse";

[@bs.get]
external getCameraProjectionMatrix : cameraT => matrix4T = "projectionMatrix";

[@bs.send] external cloneMatrix4 : matrix4T => matrix4T = "clone";

[@bs.send] external transposeMatrix4 : matrix4T => unit = "transpose";

[@bs.send]
external multiplyMatrix4 : (matrix4T, matrix4T) => unit = "multiply";

[@bs.send]
external updateMeshMatrixWorld : meshT => unit = "updateMatrixWorld";

[@bs.get] external getMeshPosition : meshT => positionT = "position";

[@bs.get] external getMeshRotation : meshT => positionT = "rotation";

[@bs.send] [@bs.scope "scale"]
external setScale : (meshT, float, float, float) => unit = "set";

[@bs.send] [@bs.scope "position"]
external setPosition : (meshT, float, float, float) => unit = "set";

[@bs.send] [@bs.scope "rotation"]
external setRotation : (meshT, float, float, float) => unit = "set";

[@bs.send] [@bs.scope "position"]
external setCameraPosition : (cameraT, float, float, float) => unit = "set";

[@bs.send] [@bs.scope "rotation"]
external setCameraRotation : (cameraT, float, float, float) => unit = "set";

[@bs.send]
external updateCameraMatrixWorld : cameraT => unit = "updateMatrixWorld";

[@bs.send] external cameraLookAt : (cameraT, meshT) => unit = "lookAt";

[@bs.send]
external cameraLookAt3 : (cameraT, float, float, float) => unit = "lookAt";

type geometryBuffersT = {
  position: Float32Array.t,
  uv: Float32Array.t,
  index: Int32Array.t
};

type objectTransformT = {matrixWorld: matrix4T};

type viewTransformT = {
  modelViewMatrix: array(float),
  projectionMatrix: array(float)
};

let protoBox = createBoxBufferGeometry(1.0, 1.0, 1.0, 1, 1, 1);

let protoBoxMaterial = createBasicMaterial();

let protoMesh = createMesh(protoBox, protoBoxMaterial);

let protoSphere = createSphereBufferGeometry(1.0, 32, 32);

let protoPlane = createPlaneBufferGeometry(1.0, 1.0, 256, 256);

let protoQuad = createPlaneBufferGeometry(1.0, 1.0, 1, 1);

let reInt32 = x => {
  /* TODO: optimize, don't expand all arrays to 32 bit */
  let ar = Int32Array.createSize(Int32Array.length(x));
  for (i in 0 to Int32Array.length(ar)) {
    Int32Array.set(ar, i, Int32Array.get(x, i));
  };
  ar;
};

let createSphereGeometry = () => {
  let box = protoSphere;
  {
    position: getFloat32Array(getPosition(getAttributes(box))),
    uv: getFloat32Array(getUV(getAttributes(box))),
    index: reInt32(getInt32Array(getIndex(box)))
  };
};

let createBoxGeometry = () => {
  let box = protoBox;
  {
    position: getFloat32Array(getPosition(getAttributes(box))),
    uv: getFloat32Array(getUV(getAttributes(box))),
    index: reInt32(getInt32Array(getIndex(box)))
  };
};

let createPlaneGeometry = () => {
  let box = protoPlane;
  {
    position: getFloat32Array(getPosition(getAttributes(box))),
    uv: getFloat32Array(getUV(getAttributes(box))),
    index: reInt32(getInt32Array(getIndex(box)))
  };
};

let createQuadGeometry = () => {
  let box = protoQuad;
  {
    position: getFloat32Array(getPosition(getAttributes(box))),
    uv: getFloat32Array(getUV(getAttributes(box))),
    index: reInt32(getInt32Array(getIndex(box)))
  };
};

let getObjectMatrix = (position, scale, rotation) => {
  let mesh = protoMesh;
  let (x, y, z) = position;
  setPosition(mesh, x, y, z);
  let (xs, ys, zs) = scale;
  setScale(mesh, xs, ys, zs);
  let (xr, yr, zr) = rotation;
  setRotation(mesh, xr, yr, zr);
  {
    matrixWorld: {
      updateMeshMatrixWorld(mesh);
      getMeshMatrixWorld(mesh);
    }
  };
};

let getCamera =
  Memoize.partialMemoize4((width, height, pos, rot) => {
    let viewAngle = 45.0;
    let aspect = float_of_int(width) /. float_of_int(height);
    let near = 0.1;
    let far = 10000.0;
    let camera = createPerspectiveCamera(viewAngle, aspect, near, far);
    let (x, y, z) = pos;
    let (rx, ry, rz) = rot;
    setCameraPosition(camera, x, y, z);
    setCameraRotation(camera, rx, ry, rz);
    camera;
  });

let getViewMatrices = (camera, matrixWorld, twist) => {
  updateCameraMatrixWorld(camera);
  let modelMatrix = cloneMatrix4(matrixWorld);
  let modelViewMatrix = cloneMatrix4(getCameraMatrixWorldInverse(camera));
  multiplyMatrix4(modelViewMatrix, modelMatrix);
  let projectionMatrix = getCameraProjectionMatrix(camera);
  let tmpArray = [|
    (-0.9366722106933594),
    0.0,
    (-0.3502073884010315),
    0.0,
    (-0.14800404012203217),
    0.9063078165054321,
    0.39585480093955994,
    0.45315390825271606,
    0.3173956871032715,
    0.4226182699203491,
    (-0.8489133715629578),
    (-3.7886908054351807),
    0.0,
    0.0,
    0.0,
    1.0
  |];
  let tmpArray2 = [|
    3.1733386516571045,
    0.0,
    0.0,
    0.0,
    0.0,
    2.4142136573791504,
    0.0,
    0.0,
    0.0,
    0.0,
    (-1.0002000331878662),
    (-0.020002000033855438),
    0.0,
    0.0,
    (-1.0),
    0.0
  |];
  let modelViewMatrix2 = cloneMatrix4(modelViewMatrix);
  let projectionMatrix2 = cloneMatrix4(projectionMatrix);
  setElements(modelViewMatrix2, tmpArray);
  /* setElements(projectionMatrix, tmpArray2);*/
  transposeMatrix4(modelViewMatrix2);
  multiplyMatrix4(modelViewMatrix2, modelMatrix);
  /* transposeMatrix4(projectionMatrix); */
  {
    modelViewMatrix:
      getElements(if (twist) {modelViewMatrix2} else {modelViewMatrix}),
    projectionMatrix: getElements(projectionMatrix)
  };
};
