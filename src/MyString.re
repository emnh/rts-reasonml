[@bs.send] external split : (string, string) => array(string) = "";

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
