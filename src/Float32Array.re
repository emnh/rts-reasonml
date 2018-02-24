type t;

[@bs.new] external create : array(float) => t = "Float32Array";

[@bs.new] external createSize : int => t = "Float32Array";

[@bs.get] external length : t => int = "length";

[@bs.get_index] external get : (t, int) => float = "";

[@bs.set_index] external set : (t, int, float) => unit = "";
