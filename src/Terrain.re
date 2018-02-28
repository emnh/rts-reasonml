let sz = 2.0;

let width = sz;

let height = sz;

let tiles = 8;

let tileWidth = tiles;

let tileHeight = tiles;

let getWidth = () => width;

let getHeight = () => height;

let getTileWidth = () => tileWidth;

let getTileHeight = () => tileHeight;

open! GLSL;

let heightBaseMultiplier = f(0.1);

let heightMultiplier = heightBaseMultiplier / ShaderCopy.maxHeight;

let getHMMul = () =>
  vec22f(
    f(1.0) / f(float_of_int(getTileWidth())),
    f(1.0) / f(float_of_int(getTileHeight()))
  );

let getIXY = objectId => {
  let tileWidth = f(float_of_int(getTileWidth()));
  let ix = fmod(objectId, tileWidth);
  let iy = floor(objectId / tileWidth);
  vec22f(ix, iy);
};

let getTiledOffset = objectId => {
  let tileWidth = f(float_of_int(getTileWidth()));
  /* TODO: get 2.0 as terrain plane width */
  (getIXY(objectId) - tileWidth / f(2.0)) * f(2.0) + f(1.0);
};
