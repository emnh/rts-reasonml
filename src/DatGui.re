type moduleT;

type datGUIDefaultT;

type datGUIT;

type controllerT;

[@bs.module "dat.gui"] external datGUIModule : moduleT = "";

/*
[@bs.get] external default : moduleT => datGUIDefaultT = "default";

[@bs.new] external datGUI : datGUIDefaultT => datGUIT = "GUI";
*/

/*
let datGUI = (moduleT) => [%bs.raw "new require('dat.gui').default.GUI()"];
*/
let datGUI : moduleT => datGUIT = [%bs.raw{|function(x){ return new DatGui.default.GUI(); }|}];

let create = () => datGUI(datGUIModule);

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
