let sz = 2.0;

let width = sz;

let height = sz;

let tileWidth = 1;

let tileHeight = 1;

let getWidth = () => width;

let getHeight = () => height;

let getTileWidth = () => tileWidth;

let getTileHeight = () => tileHeight;

let heightBaseMultiplier = GLSL.f(0.1);

let heightMultiplier = GLSL.(/)(heightBaseMultiplier, ShaderCopy.maxHeight);
