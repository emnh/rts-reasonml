type t;

[@bs.new]
external create : array(float) => t = "Float32Array";

[@bs.get]
external length : t => int = "length";
