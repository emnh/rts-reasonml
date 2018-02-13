type datGUIDefault;

type datGUIT;

type controllerT;

[@bs.val] [@bs.module "dat.gui"] external default : datGUIDefault = "default";

[@bs.new] [@bs.module "dat.gui"]
external datGUI : datGUIDefault => datGUIT = "GUI";

[@bs.send] external addFolder : (datGUIT, string) => datGUIT = "addFolder";

[@bs.send]
external addColorRGBA :
  (datGUIT, Js.Dict.t(Color.rgbaT), string) => controllerT =
  "addColor";

[@bs.send]
external onColorRGBAChange : (controllerT, Color.rgbaT => unit) => unit =
  "onChange";

[@bs.send]
external addInt :
  (datGUIT, Js.Dict.t(int), string) => controllerT =
  "addColor";

[@bs.send]
external onIntChange : (controllerT, int => unit) => unit =
  "onChange";

[@bs.send]
external addFloat :
  (datGUIT, Js.Dict.t(float), string) => controllerT =
  "addColor";

[@bs.send]
external onFloatChange : (controllerT, float => unit) => unit =
  "onChange";

[@bs.send]
external addString :
  (datGUIT, Js.Dict.t(string), string) => controllerT =
  "addColor";

[@bs.send]
external onStringChange : (controllerT, string => unit) => unit =
  "onChange";
