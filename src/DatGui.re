type datGUIDefault;

type datGUIT;

type controllerT;

[@bs.val] [@bs.module "dat.gui"] external default : datGUIDefault = "default";

[@bs.new] [@bs.module "dat.gui"]
external datGUI : datGUIDefault => datGUIT = "GUI";

[@bs.send]
external addColorRGBA :
  (datGUIT, Js.Dict.t(Color.rgbaT), string) => controllerT =
  "addColor";

[@bs.send]
external onColorRGBAChange : (controllerT, Color.rgbaT => unit) => unit =
  "onChange";

[@bs.send] external addFolder : (datGUIT, string) => datGUIT = "addFolder";
