let join = (s, l) => List.fold_left((a, b) => a ++ s ++ b, "", l);
