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

let cameraRotationX =
  Config.floatConfigVar(["camera", "rotation", "x"], 0.01, ());

let cameraRotationY =
  Config.floatConfigVar(["camera", "rotation", "y"], 0.01, ());

let cameraRotationZ =
  Config.floatConfigVar(["camera", "rotation", "z"], -5.01, ());

let cameraX = Config.floatConfigVar(["camera", "position", "x"], 0.01, ());

let cameraY = Config.floatConfigVar(["camera", "position", "y"], 0.01, ());

let cameraZ = Config.floatConfigVar(["camera", "position", "z"], 0.01, ());
