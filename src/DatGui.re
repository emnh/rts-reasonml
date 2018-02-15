type moduleT;

type datGUIDefaultT;

type datGUIT;

type controllerT;

[@bs.module "dat.gui"] external datGUIModule : moduleT = "";

[@bs.new] [@bs.scope "default"] [@bs.module "dat.gui"] external datGUI : unit => datGUIT = "GUI";

let create = () => datGUI();

[@bs.send] external addFolder : (datGUIT, string) => datGUIT = "addFolder";

[@bs.send] external destroy : datGUIT => unit = "destroy";

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
  "add";

[@bs.send]
external onIntChange : (controllerT, int => unit) => unit =
  "onChange";

[@bs.send]
external addFloat :
  (datGUIT, Js.Dict.t(float), string) => controllerT =
  "add";

[@bs.send]
external onFloatChange : (controllerT, float => unit) => unit =
  "onChange";

[@bs.send]
external addString :
  (datGUIT, Js.Dict.t(string), string) => controllerT =
  "add";

[@bs.send]
external onStringChange : (controllerT, string => unit) => unit =
  "onChange";
