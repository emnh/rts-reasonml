[@bs.val] external memoize : ((int, 'a) => 'a) = "window.memoize";

[@bs.val] external partialMemoize : ((int, array(int), 'a) => 'a) = "window.partialMemoize";
