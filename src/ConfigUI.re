let dg = ref(None);

exception DatGuiNotInitialized;

let uiFolders = Js.Dict.empty();

/* Create Dat.GUI folders if they don't exist */
let createFolders = var => {
  let rec createFolder = path => {
    let p = var#pathStr(path);
    let folder =
      switch (Js.Dict.get(uiFolders, p)) {
      | Some(folder) => folder
      | None =>
        let f =
          switch path {
          | [] =>
            switch dg^ {
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

let getNameFolderAndStorageObject = var => {
  let storageObject = Js.Dict.empty();
  let name = List.hd(List.rev(var#path));
  let _ = Js.Dict.set(storageObject, name, var#get());
  let folder = createFolders(var);
  (name, folder, storageObject);
};

let registerCreateHandlers = () => {
  let genericOnChange = (var, v) => var#set(v);
  let transformColor = (var, v) => {
    let (r, g, b, a) = v;
    let r = r + 0;
    let g = g + 0;
    let b = b + 0;
    var#set((r, g, b, a));
  };
  Config.addCreateVarCallBack(cvar => {
    let f = () => {
      let (controller, onChange) =
        switch cvar {
        | IntConfig(var) =>
          let choices =
            switch var#choices {
            | Some(Choices(x)) => Some(`IntArray(x))
            | Some(NamedChoices(x)) => Some(`IntDict(x))
            | None => None
            };
          let (name, folder, storageObject) =
            getNameFolderAndStorageObject(var);
          let storageObject = `IntDict(storageObject);
          let onChange = `IntChange(genericOnChange(var));
          let controller =
            DatGui.addChoices(folder, storageObject, name, ~choices?, ());
          (controller, onChange);
        | FloatConfig(var) =>
          let choices =
            switch var#choices {
            | Some(Choices(x)) => Some(`FloatArray(x))
            | Some(NamedChoices(x)) => Some(`FloatDict(x))
            | None => None
            };
          let (name, folder, storageObject) =
            getNameFolderAndStorageObject(var);
          let storageObject = `FloatDict(storageObject);
          let controller =
            DatGui.addChoices(folder, storageObject, name, ~choices?, ());
          let onChange = `FloatChange(genericOnChange(var));
          (controller, onChange);
        | StringConfig(var) =>
          let choices =
            switch var#choices {
            | Some(Choices(x)) => Some(`StringArray(x))
            | Some(NamedChoices(x)) => Some(`StringDict(x))
            | None => None
            };
          let (name, folder, storageObject) =
            getNameFolderAndStorageObject(var);
          let storageObject = `StringDict(storageObject);
          let onChange = `StringChange(genericOnChange(var));
          let controller =
            DatGui.addChoices(folder, storageObject, name, ~choices?, ());
          (controller, onChange);
        | ColorConfig(var) =>
          let (name, folder, storageObject) =
            getNameFolderAndStorageObject(var);
          let onChange = `ColorChange(genericOnChange(var));
          let controller =
            DatGui.addColorRGBA(folder, storageObject, name);
          (controller, onChange);
        };
      DatGui.onChange(controller, onChange);
    };
    Document.onReady(f);
  });
};

let init = () => {
  registerCreateHandlers();
  let datgui = DatGui.create();
  Document.debug(Document.window, datgui);
  dg := Some(datgui);
};

let destroy = () =>
  switch dg^ {
  | Some(x) => DatGui.destroy(x)
  | None => ()
  };
