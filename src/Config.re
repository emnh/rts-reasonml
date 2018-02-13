type colorT = (float, float, float);

type configT =
  | IntConfig
  | FloatConfig
  | StringConfig
  | ColorConfig;

type configVar('a) = {
  get: unit => 'a,
  set: 'a => unit,
  registerUpdate: ('a => unit) => unit
};

/*
 let getColorConfig = var => (
   LocalStorage.getFloat(var#path ++ ".r"),
   LocalStorage.getFloat(var#path ++ ".g"),
   LocalStorage.getFloat(var#path ++ ".b")
 );

 let setColorConfig = (var, (r, g, b)) => (
   LocalStorage.setFloat(var#path ++ ".r", r),
   LocalStorage.setFloat(var#path ++ ".g", g),
   LocalStorage.setFloat(var#path ++ ".b", b)
 );
 */
let intConfigVar = (path, defaultValue) => {
  let pathStr = String.concat("/", path);
  let update = ref([]);
  let get = () => {
    let value = LocalStorage.getInt(pathStr);
    switch value {
    | Some(v) => v
    | None => defaultValue
    };
  };
  let var: configVar(int) = {
    get,
    set: value => {
      List.iter(f => f(value), update^);
      LocalStorage.setInt(pathStr, value);
    },
    registerUpdate: f => {
      f(get());
      update := [f, ...update^];
      ();
    }
  };
  /* Set default value and call updaters */
  var.set(var.get());
  var;
};

let a = intConfigVar(["configtest"], 3);
/* let canvasBackgroundColor = intConfigVar(["canvas", "background", "color"], 0);
 * */
