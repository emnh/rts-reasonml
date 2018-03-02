[@bs.send] external split : (string, string) => array(string) = "";

[@bs.send] external indexOf : (string, string) => int = "";

[@bs.send] external replace : (string, string, string) => string = "";

[@bs.send] external replaceRe : (string, Js.Re.t, string) => string = "replace";

let lineNumbers = s => {
  let l = Array.to_list(split(s, "\n"));
  let c = ref(0);
  List.fold_left(
    (x, y) => {
      c := c^ + 1;
      x ++ "\n" ++ string_of_int(c^) ++ ": " ++ y;
    },
    "",
    l
  );
};
