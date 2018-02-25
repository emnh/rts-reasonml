type t;

[@bs.new] external create : array(float) => t = "Int16Array";

[@bs.new] external createSize : int => t = "Int16Array";

[@bs.get] external length : t => int = "length";

[@bs.get_index] external get : (t, int) => int = "";

[@bs.set_index] external set : (t, int, int) => unit = "";
