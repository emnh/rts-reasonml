/* #version 300 es

   // an attribute is an input (in) to a vertex shader.
   // It will receive data from a buffer
   in vec4 a_position;

   // all shaders have a main function
   void main() {

     // gl_Position is a special variable a vertex shader
     // is responsible for setting
     gl_Position = a_position;
   } */
let version = "#version 300 es";

let newline = "\n";

let indent = "  ";

exception GLSLTypeError(string);

type glslTypeT =
  | Void
  | Int
  | Float
  | Vec2
  | Vec3
  | Vec4;

type varT =
  | Builtin
  | Attribute
  | Uniform
  | Variable;

type varExprT = (varT, glslTypeT, string);

include GLSLSwizzleFormat;

type commonExprT =
  | Var(varExprT)
  | Swizzle(commonExprT, swizzleT);

type leftExprT = commonExprT;

/* Translated from https://github.com/kovasb/gamma/blob/master/src/gamma/ast.cljs */
type rT =
  /* | TypeError */
  | Common(commonExprT)
  | Inc(rT)
  | Dec(rT)
  | PreInc(rT)
  | PreDec(rT)
  | Not(rT)
  /* | PlusMinus(rT) */
  | Mul(rT, rT)
  | Div(rT, rT)
  | Plus(rT, rT)
  | Minus(rT, rT)
  | LessThan(rT, rT)
  | GreaterThan(rT, rT)
  | LessThanOrEqual(rT, rT)
  | GreaterThanOrEqual(rT, rT)
  | Equal(rT, rT)
  | NotEqual(rT, rT)
  | And(rT, rT)
  | Or(rT, rT)
  | Xor(rT, rT);

type rightExprT = rT;

type statementT =
  | Assignment(leftExprT, rightExprT)
  | Discard;

type gRootT = list(statementT);

type nodeT =
  | StatementNode(statementT)
  | LExprNode(leftExprT)
  | RExprNode(rightExprT)
  | CommonExprNode(commonExprT)
  | VarNode(varExprT)
  | SwizzleNode(swizzleT);

let glslTypeString = t =>
  switch t {
  | Void => "void"
  | Int => "int"
  | Float => "float"
  | Vec2 => "vec2"
  | Vec3 => "vec3"
  | Vec4 => "vec4"
  };

type transformer('a) = {
  combine: (transformer('a), list('a)) => 'a,
  rexprCombine: (transformer('a), list('a)) => 'a,
  common: (transformer('a), commonExprT) => 'a,
  lExpr: (transformer('a), leftExprT) => 'a,
  rExpr: (transformer('a), rightExprT) => 'a,
  tree: (transformer('a), list(statementT)) => 'a,
  tfun: (transformer('a), (glslTypeT, string, unit => list(statementT))) => 'a
};

let combine = (_, l) => List.fold_left((++), "", l);

let fmtTransformer = {
  combine,
  rexprCombine: (t, l) => t.combine(t, ["(", t.combine(t, l), ")"]),
  common: (t, expr) =>
    t.combine(
      t,
      switch expr {
      | Var((_, _, name)) => [name]
      | Swizzle(expr, swizzle) => [t.common(t, expr), ".", fmtSwizzle(swizzle)]
      }
    ),
  lExpr: (t, expr) => t.common(t, expr),
  rExpr: (t, expr) =>
    t.rexprCombine(
      t,
      switch expr {
      | Common(e) => [t.common(t, e)]
      | Inc(l) => ["(", t.rExpr(t, l), "++)"]
      | Dec(l) => ["(", t.rExpr(t, l), "--)"]
      | PreInc(l) => ["(++", t.rExpr(t, l), ")"]
      | PreDec(l) => ["(--", t.rExpr(t, l), ")"]
      | Not(l) => ["(!", t.rExpr(t, l), ")"]
      | Mul(l, r) => ["(", t.rExpr(t, l), " * ", t.rExpr(t, r), ")"]
      | Div(l, r) => ["(", t.rExpr(t, l), " / ", t.rExpr(t, r), ")"]
      | Plus(l, r) => ["(", t.rExpr(t, l), " + ", t.rExpr(t, r), ")"]
      | Minus(l, r) => ["(", t.rExpr(t, l), " - ", t.rExpr(t, r), ")"]
      | LessThan(l, r) => ["(", t.rExpr(t, l), " < ", t.rExpr(t, r), ")"]
      | GreaterThan(l, r) => ["(", t.rExpr(t, l), " > ", t.rExpr(t, r), ")"]
      | LessThanOrEqual(l, r) => [
          "(",
          t.rExpr(t, l),
          " <= ",
          t.rExpr(t, r),
          ")"
        ]
      | GreaterThanOrEqual(l, r) => [
          "(",
          t.rExpr(t, l),
          " >= ",
          t.rExpr(t, r),
          ")"
        ]
      | Equal(l, r) => ["(", t.rExpr(t, l), " == ", t.rExpr(t, r), ")"]
      | NotEqual(l, r) => ["(", t.rExpr(t, l), " != ", t.rExpr(t, r), ")"]
      | And(l, r) => ["(", t.rExpr(t, l), " && ", t.rExpr(t, r), ")"]
      | Or(l, r) => ["(", t.rExpr(t, l), " || ", t.rExpr(t, r), ")"]
      | Xor(l, r) => ["(", t.rExpr(t, l), " ^^ ", t.rExpr(t, r), ")"]
      }
    ),
  tree: (t, tree) =>
    t.combine(
      t,
      List.map(
        stmt =>
          t.combine(
            t,
            [
              indent,
              switch stmt {
              | Assignment(left, right) =>
                t.combine(t, [t.lExpr(t, left), " = ", t.rExpr(t, right)])
              | Discard => "discard"
              },
              ";",
              newline
            ]
          ),
        tree
      )
    ),
  tfun: (t, gf) => {
    let (tt, name, astf) = gf;
    let tree = astf();
    t.combine(
      t,
      [glslTypeString(tt), " ", name, "() {", newline, t.tree(t, tree), "}"]
    );
  }
};

let fmtFun = gf => fmtTransformer.tfun(fmtTransformer, gf);

let attr = (t, name) => Var((Attribute, t, name));

let builtin = (t, name) => Var((Builtin, t, name));

let gfun = (t, name, ast) => (t, name, ast);

module SS = Set.Make(String);

let formatAttribute = attr => {
  let (t, name) = attr;
  "in " ++ glslTypeString(t) ++ " " ++ name ++ ";";
};

let getAttributes = gf => {
  let ar = ref(SS.empty);
  let walkTransformer = {
    ...fmtTransformer,
    common: (t, expr) => {
      switch expr {
      | Var((Attribute, t, name)) =>
        ar := SS.add(formatAttribute((t, name)), ar^)
      | _ => ()
      };
      fmtTransformer.common(t, expr);
    }
  };
  let _ = walkTransformer.tfun(walkTransformer, gf);
  ar^;
};

let formatAttributes = attrs => MyString.join(newline, SS.elements(attrs));

let assign = (dest, src) => {
  let dest =
    switch dest {
    | Common(x) => x
    | _ => raise(GLSLTypeError("left hand side must be common expression"))
    };
  Assignment(dest, src);
};

let ( **. ) = (var, st) => Common(Swizzle(var, st));

let symCounter = ref(0);

let genSym = () => {
  symCounter := symCounter^ + 1;
  "v_" ++ string_of_int(symCounter^);
};

type fvec3 = {
  varExpr: varExprT,
  self: rightExprT,
  x: rightExprT,
  y: rightExprT,
  xyz: rightExprT
};

type fvec4 = {
  varExpr: varExprT,
  self: rightExprT,
  x: rightExprT,
  y: rightExprT,
  xyz: rightExprT,
  xyzw: rightExprT
};

let vec3 = (vart, name) => {
  let varExpr = (vart, Vec3, name);
  {
    varExpr,
    self: Common(Var(varExpr)),
    x: Common(Swizzle(Var(varExpr), X)),
    y: Common(Swizzle(Var(varExpr), Y)),
    xyz: Common(Swizzle(Var(varExpr), XYZ))
  };
};

let vec3attr = (name) => vec3(Attribute, name);

let vec4 = (vart, name) => {
  let varExpr = (vart, Vec4, name);
  {
    varExpr,
    self: Common(Var(varExpr)),
    x: Common(Swizzle(Var(varExpr), X)),
    y: Common(Swizzle(Var(varExpr), Y)),
    xyz: Common(Swizzle(Var(varExpr), XYZ)),
    xyzw: Common(Swizzle(Var(varExpr), XYZW))
  };
};

let vec4attr = (name) => vec4(Attribute, name);

let gl_Position = vec4(Builtin, "gl_Position");

/* One specific shader */

let position3 = vec3attr("a_position");

let position4 = vec4attr("a_position");

module Fragment = {
  module type ElementType = {
    let add: statementT => unit;
    let finish: unit => list(statementT);
  };
  module ElementModule: ElementType = {
    let ar = ref([]);
    let add = x => {
      ar := [x, ...ar^];
      ();
    };
    let finish = () => {
      let value = List.rev(ar^);
      ar := [];
      value;
    };
  };
  module Make = (Element: ElementType) => {
    include Element;
    let (=@) = (l, r) => add(assign(l, r));
    let (++) = (l) => Inc(l);
    let (--) = (l) => Dec(l);
    let (+++) = (l) => PreInc(l);
    let (---) = (l) => PreDec(l);
    let (!) = (l) => Not(l);
    let ( * ) = (l, r) => Mul(l, r);
    let (/) = (l, r) => Div(l, r);
    let (+) = (l, r) => Plus(l, r);
    let (-) = (l, r) => Minus(l, r);
    let (<) = (l, r) => LessThan(l, r);
    let (>) = (l, r) => GreaterThan(l, r);
    let (<=) = (l, r) => LessThanOrEqual(l, r);
    let (>=) = (l, r) => GreaterThanOrEqual(l, r);
    let (==) = (l, r) => Equal(l, r);
    let (!=) = (l, r) => NotEqual(l, r);
    let (&&) = (l, r) => And(l, r);
    let (||) = (l, r) => Or(l, r);
    let (^^) = (l, r) => Xor(l, r);
  };
};

let main =
  gfun(
    Void,
    "main",
    () => {
      module Frag1 = Fragment.Make(Fragment.ElementModule);
      open! Frag1;
      /* gl_Position is a special variable a vertex shader is responsible for setting */
      gl_Position.xyzw =@ position4.x + position4.y;
      gl_Position.xyzw =@ position3.xyz + position3.xyz;
      gl_Position.xyzw =@ position4.xyzw + position4.xyzw;
      gl_Position.self =@ position4.xyzw + position4.xyzw;
      finish();
    }
  );

let getShader = main =>
  version
  ++ newline
  ++ formatAttributes(getAttributes(main))
  ++ newline
  ++ newline
  ++ fmtFun(main);

let shader = getShader(main);
