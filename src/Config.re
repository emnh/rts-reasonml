type configVar('a) = {
  path: list(string),
  pathStr: list(string) => string,
  get: unit => 'a,
  set: 'a => unit,
  registerUpdate: ('a => unit) => unit
};

type rgbaT = Color.rgbaT;

type configT =
  | IntConfig(configVar(int))
  | FloatConfig(configVar(float))
  | StringConfig(configVar(string))
  | ColorConfig(configVar(rgbaT));

let createVarCallbacks = ref([]);

let addCreateVarCallBack = f =>
  createVarCallbacks := [f, ...createVarCallbacks^];

let configVar = (path, defaultValue, getValue, setValue, wrap) => {
  let pathStr = p => String.concat("/", p);
  let update = ref([]);
  let getInitial = () => {
    let value = getValue(pathStr(path));
    switch value {
    | Some(v) => v
    | None => defaultValue
    };
  };
  let cached = ref(getInitial());
  let get = () => cached^;
  let var = {
    path,
    pathStr,
    get,
    set: value => {
      cached := value;
      setValue(pathStr(path), value);
      List.iter(f => f(value), update^);
    },
    registerUpdate: f => {
      f(get());
      update := [f, ...update^];
      ();
    }
  };
  /* Set default/stored value and call updaters */
  var.set(var.get());
  /* Call config variable creation callbacks */
  List.iter(f => f(wrap(var)), createVarCallbacks^);
  var;
};

let colorConfigVar = (path, defaultValue) => {
  let getColor = path => {
    let r = LocalStorage.getInt(path ++ ".r");
    let g = LocalStorage.getInt(path ++ ".g");
    let b = LocalStorage.getInt(path ++ ".b");
    let a = LocalStorage.getFloat(path ++ ".a");
    switch (r, g, b, a) {
    | (Some(xr), Some(xg), Some(xb), Some(xa)) => Some((xr, xg, xb, xa))
    | _ => None
    };
  };
  let setColor = (path, (r, g, b, a)) => {
    LocalStorage.setInt(path ++ ".r", r + 0);
    LocalStorage.setInt(path ++ ".g", g + 0);
    LocalStorage.setInt(path ++ ".b", b + 0);
    LocalStorage.setFloat(path ++ ".a", a);
    ();
  };
  let wrap = var => ColorConfig(var);
  configVar(path, defaultValue, getColor, setColor, wrap);
};

let intConfigVar = (path, defaultValue) =>
  configVar(path, defaultValue, LocalStorage.getInt, LocalStorage.setInt, var =>
    IntConfig(var)
  );
/* let canvasBackgroundColor = intConfigVar(["canvas", "background", "color"], 0);
 * */
