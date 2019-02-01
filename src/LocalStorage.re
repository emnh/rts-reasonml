[@bs.val]
external set : (string, string) => unit = "window.localStorage.setItem";

[@bs.val]
external get : string => Js.Nullable.t(string) = "window.localStorage.getItem";

let setInt = (name, value) => set(name, string_of_int(value));

let setFloat = (name, value) => set(name, string_of_float(value));

let setString = (name, value) => set(name, value);

let getInt = name => {
  let value = Js.Nullable.toOption(get(name));
  switch value {
  | Some(x) => Some(int_of_string(x))
  | None => None
  };
};

let getFloat = name => {
  let value = Js.Nullable.toOption(get(name));
  switch value {
  | Some(x) => Some(float_of_string(x))
  | None => None
  };
};

let getString = name => {
  let value = Js.Nullable.toOption(get(name));
  value;
};
