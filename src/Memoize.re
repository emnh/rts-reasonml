type optionsT;

[@bs.val] external memoize : (int, 'a) => 'a = "window.memoize";

[@bs.val]
external partialMemoize11 :
  ([@bs.uncurry] ('a => 'r)) => [@bs.uncurry] ('a => 'r) =
  "window.partialMemoize1";

[@bs.val]
external partialMemoize22 :
  ([@bs.uncurry] (('a, 'b) => 'r)) => [@bs.uncurry] (('a, 'b) => 'r) =
  "window.partialMemoize2";

[@bs.val]
external partialMemoize33 :
  ([@bs.uncurry] (('a, 'b, 'c) => 'r)) => [@bs.uncurry] (('a, 'b, 'c) => 'r) =
  "window.partialMemoize3";

[@bs.val] external partialMemoize0 : 'a => 'a = "window.partialMemoize0";

[@bs.val] external partialMemoize1 : 'a => 'a = "window.partialMemoize1";

[@bs.val] external partialMemoize2 : 'a => 'a = "window.partialMemoize2";

[@bs.val] external partialMemoize3 : 'a => 'a = "window.partialMemoize3";

[@bs.val] external partialMemoize4 : 'a => 'a = "window.partialMemoize4";

[@bs.val]
external partialMemoize44 :
  ([@bs.uncurry] (('a, 'b, 'c, 'd) => 'r)) =>
  [@bs.uncurry] (('a, 'b, 'c, 'd) => 'r) =
  "window.partialMemoize4";

[@bs.get]
external getMemoizeIdentity : 'a => Js.Undefined.t(float) = "memoizeId";

[@bs.set] external setMemoizeIdentity : ('a, float) => unit = "memoizeId";

/* TODO: Use global identifier instead of Math.random */
let setMemoizeId = x => {
  let om = getMemoizeIdentity(x);
  if (om == Js.undefined) {
    setMemoizeIdentity(x, Math.random());
  };
};
