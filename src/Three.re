type materialT;

type geometryT;

type boxT = geometryT;

type meshT;

type attributesT;

type arrayBufferT;

type cameraT;

type matrix4T;

type vector3T;

type positionT = vector3T;

[@bs.new] [@bs.module "three"]
external createBoxBufferGeometry : (float, float, float) => boxT =
  "BoxBufferGeometry";

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

[@bs.send] external multiplyMatrix4 : (matrix4T, matrix4T) => unit = "multiply";

[@bs.send] external updateMeshMatrixWorld : meshT => unit = "updateMatrixWorld";

[@bs.get] external getMeshPosition : meshT => positionT = "position";

[@bs.get] external getMeshRotation : meshT => positionT = "rotation";

[@bs.send]
external setPosition : (positionT, float, float, float) => unit = "set";

[@bs.send]
external setRotation : (positionT, float, float, float) => unit = "set";

[@bs.send]
external updateCameraMatrixWorld : cameraT => unit = "updateMatrixWorld";

type geometryBuffersT = {
  mesh: meshT,
  box: boxT,
  position: Float32Array.t,
  uv: Float32Array.t,
  index: Int16Array.t,
  matrixWorld: unit => matrix4T
};

type viewTransformT = {
  modelViewMatrix: array(float),
  projectionMatrix: array(float)
};

let createBox = (x, y, z, xr, yr, zr) => {
  let box = createBoxBufferGeometry(x, y, z);
  let material = createBasicMaterial();
  let mesh = createMesh(box, material);
  setPosition(getMeshPosition(mesh), 0.0, 0.0, -10.0);
  setRotation(getMeshRotation(mesh), xr, yr, zr);
  {
    mesh,
    box,
    position: getFloat32Array(getPosition(getAttributes(box))),
    uv: getFloat32Array(getUV(getAttributes(box))),
    index: getInt16Array(getIndex(box)),
    matrixWorld: () => {
      updateMeshMatrixWorld(mesh);
      getMeshMatrixWorld(mesh);
    }
  };
};

let getViewMatrices = (matrixWorld, width, height) => {
  let viewAngle = 45.0;
  let aspect = float_of_int(width) /. float_of_int(height);
  let near = 0.1;
  let far = 10000.0;
  let camera = createPerspectiveCamera(viewAngle, aspect, near, far);
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