type datGUIDefault;

type datGUIT;

[@bs.val] [@bs.module "dat.gui"] external default : datGUIDefault = "default";

[@bs.new] [@bs.module "dat.gui"]
external datGUI : datGUIDefault => datGUIT = "GUI";
