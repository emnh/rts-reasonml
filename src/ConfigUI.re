let dg = ref(None);

exception DatGuiNotInitialized;

let uiFolders = Js.Dict.empty();

/* Create Dat.GUI folders if they don't exist */
let createFolders = (var : Config.configVarT('a)) => {
  let rec createFolder = path => {
    let p = var#pathStr(path);
    let folder =
      switch (Js.Dict.get(uiFolders, p)) {
      | Some(folder) => folder
      | None =>
        let f =
          switch path {
          | [] => switch dg^ {
            | Some(dg) => dg
            | None => raise(DatGuiNotInitialized)
            }
          | [head, ...tail] =>
            let folder = createFolder(tail);
            DatGui.addFolder(folder, head);
          };
        let _ = Js.Dict.set(uiFolders, p, f);
        f;
      };
    folder;
  };
  createFolder(List.tl(List.rev(var#path)));
};

let addUIVar = (var : Config.configVarT('a), addF, changeF, onChange) => {
  let guiObj = Js.Dict.empty();
  let name = List.hd(List.rev(var#path));
  let _ = Js.Dict.set(guiObj, name, var#get());
  let folder = createFolders(var);
  let controller = addF(folder, guiObj, name);
  changeF(controller, onChange);
};

let registerCreateHandlers = () => {
  let onChange = (var, v) => var#set(v);
  let transformColor = (var) => v => {
    let (r, g, b, a) = v;
    let r = r + 0;
    let g = g + 0;
    let b = b + 0;
    var#set((r, g, b, a));
  };
  Config.addCreateVarCallBack(cvar => {
    switch cvar {
      | IntConfig(var) => addUIVar(var, DatGui.addInt, DatGui.onIntChange, onChange(var))
      | FloatConfig(var) => addUIVar(var, DatGui.addFloat, DatGui.onFloatChange, onChange(var))
      | StringConfig(var) => addUIVar(var, DatGui.addString, DatGui.onStringChange, onChange(var))
      | ColorConfig(var) => addUIVar(var, DatGui.addColorRGBA, DatGui.onColorRGBAChange, transformColor(var))
    }
  });
};

let init = () => {
  dg := Some(DatGui.create());
};
