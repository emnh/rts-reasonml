type choiceT('a) =
  | Choices(array('a))
  | NamedChoices(Js.Dict.t('a));

type configVarT('a) = {
  .
  path: list(string),
  pathStr: list(string) => string,
  get: unit => 'a,
  set: 'a => unit,
  registerUpdate: ('a => unit) => unit,
  choices: option(choiceT('a))
};

type rgbaT = Color.rgbaT;

type configT =
  | IntConfig(configVarT(int))
  | FloatConfig(configVarT(float))
  | StringConfig(configVarT(string))
  | ColorConfig(configVarT(rgbaT));

let configVars = ref([]);

let createVarCallbacks = ref([]);

let addCreateVarCallBack = f =>
{
  /** Call on already created vars **/
  List.iter(var => f(var), List.rev(configVars^));
  /** Add it to the list **/
  createVarCallbacks := [f, ...createVarCallbacks^];
};

let configVar = (path, defaultValue, getValue, setValue, wrap, ~choices=?, ()) => {
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
    pri blah = this;
    pub path = path;
    pub pathStr = pathStr;
    pub get = get;
    pub set = value => {
      cached := value;
      setValue(pathStr(path), value);
      List.iter(f => f(value), update^);
    };
    pub registerUpdate = f => {
      f(get());
      update := [f, ...update^];
      ();
    };
    pub choices = choices
  };
  /* Set default/stored value and call updaters */
  var#set(var#get());
  /* Call config variable creation callbacks */
  List.iter(f => f(wrap(var)), createVarCallbacks^);
  /* Save created vars */
  configVars := [wrap(var), ...configVars^];
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

let intConfigVar = (path, defaultValue, ~choices=?, ()) =>
  configVar(
    path,
    defaultValue,
    LocalStorage.getInt,
    LocalStorage.setInt,
    var => IntConfig(var),
    ~choices?,
    ()
  );

let floatConfigVar = (path, defaultValue, ~choices=?, ()) =>
  configVar(
    path,
    defaultValue,
    LocalStorage.getFloat,
    LocalStorage.setFloat,
    var => FloatConfig(var),
    ~choices?,
    ()
  );

let stringConfigVar = (path, defaultValue, ~choices=?, ()) =>
  configVar(
    path,
    defaultValue,
    LocalStorage.getString,
    LocalStorage.setString,
    var => StringConfig(var),
    ~choices?,
    ()
  );
