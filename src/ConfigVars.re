let geometryType =
  Config.stringConfigVar(
    ["object", "geometry"],
    "Box",
    ~choices=Config.Choices([|"Box", "Sphere", "Plane"|]),
    ()
  );

let backgroundColor =
  Config.colorConfigVar(["object", "color1"], (0, 0, 0, 1.0), ());

let foregroundColor =
  Config.colorConfigVar(["object", "color2"], (0, 0, 0, 1.0), ());

let seed = Config.stringConfigVar(["object", "seed"], "hello", ());

let spread = Config.floatConfigVar(["object", "spread"], 2.01, ());

let rotationSpread =
  Config.floatConfigVar(["object", "rotationSpread"], Math.pi, ());

let rotationSpeed =
  Config.floatConfigVar(["object", "rotationSpeed"], 1.0, ());

let count = Config.intConfigVar(["object", "count"], 100, ());

let size = Config.floatConfigVar(["object", "size"], 1.5, ());

let cameraRotationX =
  Config.floatConfigVar(["camera", "rotation", "x"], 0.01, ());

let cameraRotationY =
  Config.floatConfigVar(["camera", "rotation", "y"], 0.01, ());

let cameraRotationZ =
  Config.floatConfigVar(["camera", "rotation", "z"], -5.01, ());

let cameraX = Config.floatConfigVar(["camera", "position", "x"], 0.01, ());

let cameraY = Config.floatConfigVar(["camera", "position", "y"], 0.01, ());

let cameraZ = Config.floatConfigVar(["camera", "position", "z"], 0.01, ());

let waterHeight = Config.floatConfigVar(["water", "height"], 0.1, ());

let waveHeight = Config.floatConfigVar(["water", "waveHeight"], 0.5, ());

let objectX = Config.floatConfigVar(["object", "position", "x"], 0.01, ());

let objectY = Config.floatConfigVar(["object", "position", "y"], 0.01, ());

let objectZ = Config.floatConfigVar(["object", "position", "z"], 0.01, ());

let lightX = Config.floatConfigVar(["light", "position", "x"], 0.01, ());

let lightY = Config.floatConfigVar(["light", "position", "y"], 0.01, ());

let lightZ = Config.floatConfigVar(["light", "position", "z"], 0.01, ());

let eyeX = Config.floatConfigVar(["camera", "eye", "x"], 0.01, ());

let eyeY = Config.floatConfigVar(["camera", "eye", "y"], 0.01, ());

let eyeZ = Config.floatConfigVar(["camera", "eye", "z"], 0.01, ());

let poolHeight = Config.floatConfigVar(["water", "poolHeight"], 1.5, ());

let terrainScale = Config.floatConfigVar(["terrain", "scale"], 0.25, ());
