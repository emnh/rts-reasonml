type exprT =
  | Lit(int)
  | Add(exprT, exprT)
  | Neg(exprT);

module type ExprRepr = {
  type t;
  let add: (t, t) => t;
  let neg: t => t;
  let lit: int => t;
};

module ExprEval: (ExprRepr with type t = int) = {
  type t = int;
  let add = (x, y) => x + y;
  let neg = x => - x;
  let lit = x => x;
};

module ExprConstruct : (ExprRepr with type t = exprT) = {
  type t = exprT;
  let add = (x, y) => Add(x, y);
  let neg = x => Neg(x);
  let lit = x => Lit(x);
};

module type ExprViewString = {
  type t = string;
  let expr : exprT => t;
};

module ExprView = (Self: ExprViewString) => {
  let add = (x, y) => "add(" ++ x ++ "," ++ y ++ ")";
  let neg = (x) => "neg(" ++ x ++ ")";
  let lit = (x) => string_of_int(x);
  let expr = e =>
    switch (e) {
    | Add(x, y) => add(Self.expr(x), Self.expr(y))
    | Neg(x) => neg(Self.expr(x));
    | Lit(x) => lit(x);
    };
};

open ExprConstruct;

let x = add(lit(8), neg(add(lit(2), lit(3))));

Js.log(x);

