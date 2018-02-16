let geometryType =
  Config.stringConfigVar(
    ["object", "geometry"],
    "Box",
    ~choices=Config.Choices([|"Box", "Sphere", "Plane"|]),
    ()
  );

let foregroundColor =
  Config.colorConfigVar(["object", "color2"], (0, 0, 0, 1.0), ());

let backgroundColor =
  Config.colorConfigVar(["object", "color1"], (0, 0, 0, 1.0), ());

let cameraX = Config.floatConfigVar(["camera", "position", "x"], 0.0, ());
let cameraY = Config.floatConfigVar(["camera", "position", "y"], 0.0, ());
let cameraZ = Config.floatConfigVar(["camera", "position", "z"], 0.0, ());
