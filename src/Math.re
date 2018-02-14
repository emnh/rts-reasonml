let pi = [%bs.raw "Math.PI"];

[@bs.val] external random : unit => float = "Math.random";

[@bs.val] external sin : float => float = "Math.sin";

[@bs.val] external cos : float => float = "Math.cos";

