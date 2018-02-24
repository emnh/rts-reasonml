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

[@bs.get] external getInt16Array : arrayBufferT => Int16Array.t = "array";

[@bs.get] external getIndex : boxT => arrayBufferT = "index";

[@bs.get] external getElements : matrix4T => array(float) = "elements";

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

type geometryBuffersT = {
  position: Float32Array.t,
  uv: Float32Array.t,
  index: Int16Array.t
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

let createSphereGeometry = () => {
  let box = protoSphere;
  {
    position: getFloat32Array(getPosition(getAttributes(box))),
    uv: getFloat32Array(getUV(getAttributes(box))),
    index: getInt16Array(getIndex(box))
  };
};

let createBoxGeometry = () => {
  let box = protoBox;
  {
    position: getFloat32Array(getPosition(getAttributes(box))),
    uv: getFloat32Array(getUV(getAttributes(box))),
    index: getInt16Array(getIndex(box))
  };
};

let createPlaneGeometry = () => {
  let box = protoPlane;
  {
    position: getFloat32Array(getPosition(getAttributes(box))),
    uv: getFloat32Array(getUV(getAttributes(box))),
    index: getInt16Array(getIndex(box))
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

let getViewMatrices = (camera, matrixWorld) => {
  updateCameraMatrixWorld(camera);
  let modelMatrix = cloneMatrix4(matrixWorld);
  let modelViewMatrix = cloneMatrix4(getCameraMatrixWorldInverse(camera));
  multiplyMatrix4(modelViewMatrix, modelMatrix);
  let projectionMatrix = getCameraProjectionMatrix(camera);
  /*
   transposeMatrix4(modelViewMatrix);
   transposeMatrix4(projectionMatrix);*/
  {
    modelViewMatrix: getElements(modelViewMatrix),
    projectionMatrix: getElements(projectionMatrix)
  };
};
