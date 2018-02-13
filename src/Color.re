type rgbaT = (int, int, int, float);

let stringColor = rgba => {
  let (r, g, b, _) = rgba;
  "rgb("
  ++ string_of_int(r)
  ++ ","
  ++ string_of_int(g)
  ++ ","
  ++ string_of_int(b)
  ++ ")";
};

let setA = (rgba, a) => {
  let (r, g, b, _) = rgba;
  (r, g, b, a);
};
