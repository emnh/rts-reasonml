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

type swizzleT =
  | X
  | Y
  | Z
  | W
  | XY
  | XYZ
  | XYZW;

type commonExprT =
  | Var(varExprT)
  | Swizzle(commonExprT, swizzleT);

type leftExprT = commonExprT;

type rightExprT =
  | Common(commonExprT)
  | Plus(rightExprT, rightExprT)
  | Minus(rightExprT, rightExprT)
  | Mul(rightExprT, rightExprT)
  | Div(rightExprT, rightExprT);

type statementT =
  | Assignment(leftExprT, rightExprT)
  | Discard;

type gRootT = list(statementT);

type nodeT =
  | StatementNode(statementT)
  | LExprNode(leftExprT)
  | RExprNode(rightExprT)
  | CommonExprNode(commonExprT)
  | VarNode(varExprT);

let glslTypeString = t =>
  switch t {
  | Void => "void"
  | Int => "int"
  | Float => "float"
  | Vec2 => "vec2"
  | Vec3 => "vec3"
  | Vec4 => "vec4"
  };

let rec walkCommonExpr = (tree, f) =>
  switch tree {
  | Var(e) =>
    f(CommonExprNode(Var(e)));
    f(VarNode(e));
  | Swizzle(e, se) =>
    f(CommonExprNode(e));
    walkCommonExpr(e, f);
  };

let walkLExpr = (tree, f) => walkCommonExpr(tree, f);

let rec walkRExpr = (tree, f) =>
  switch tree {
  | Common(expr) =>
    f(CommonExprNode(expr));
    walkCommonExpr(expr, f);
  | Plus(l, r) =>
    f(RExprNode(l));
    f(RExprNode(r));
    walkRExpr(r, f);
  | Minus(l, r) =>
    f(RExprNode(l));
    f(RExprNode(r));
    walkRExpr(r, f);
  | Mul(l, r) =>
    f(RExprNode(l));
    f(RExprNode(r));
    walkRExpr(r, f);
  | Div(l, r) =>
    f(RExprNode(l));
    f(RExprNode(r));
    walkRExpr(r, f);
  };

let walkTree = (tree, f) =>
  List.iter(
    stmt =>
      /* f(Statement(stmt)); */
      switch stmt {
      | Assignment(left, right) =>
        f(LExprNode(left));
        f(RExprNode(right));
        walkLExpr(left, f);
        walkRExpr(right, f);
        ();
      | Discard =>
        f(StatementNode(Discard));
        ();
      },
    tree
  );

let fmtSwizzle = swizzle =>
  switch swizzle {
  | X => "x"
  | Y => "y"
  | Z => "z"
  | W => "w"
  | XY => "xy"
  | XYZ => "xyz"
  | XYZW => "xyzw"
  };

let rec fmtCommon = expr =>
  switch expr {
  | Var((_, _, name)) => name
  | Swizzle(expr, swizzle) => fmtCommon(expr) ++ "." ++ fmtSwizzle(swizzle)
  };

let rec fmtRExpr = rexpr =>
  switch rexpr {
  | Common(expr) => fmtCommon(expr)
  | Plus(l, r) => fmtRExpr(l) ++ " + " ++ fmtRExpr(r)
  | Minus(l, r) => fmtRExpr(l) ++ " - " ++ fmtRExpr(r)
  | Mul(l, r) => fmtRExpr(l) ++ " * " ++ fmtRExpr(r)
  | Div(l, r) => fmtRExpr(l) ++ " / " ++ fmtRExpr(r)
  };

let fmtLExpr = lexpr => fmtCommon(lexpr);

let fmtTree = tree =>
  String.join(
    "",
    List.map(
      stmt =>
        indent
        ++ (
          switch stmt {
          | Assignment(left, right) =>
            fmtLExpr(left) ++ " = " ++ fmtRExpr(right)
          | Discard => "discard"
          }
        )
        ++ ";"
        ++ newline,
      tree
    )
  );

let fmtFun = gf => {
  let (t, name, astf) = gf;
  let tree = astf();
  glslTypeString(t) ++ " " ++ name ++ "() {" ++ newline ++ fmtTree(tree) ++ "}";
};

let attr = (t, name) => Var((Attribute, t, name));

let builtin = (t, name) => Var((Builtin, t, name));

let gfun = (t, name, ast) => (t, name, ast);

let getAttributes = gf => {
  let (_, _, astf) = gf;
  let tree = astf();
  let ar = ref([]);
  let f = x =>
    switch x {
    | VarNode((Attribute, t, name)) =>
      ar := [(t, name), ...ar^];
      ();
    | _ => ()
    };
  walkTree(tree, f);
  ar^;
};

let formatAttribute = attr => {
  let (t, name) = attr;
  "in " ++ glslTypeString(t) ++ " " ++ name ++ ";";
};

let formatAttributes = attrs =>
  String.join(newline, List.map(formatAttribute, attrs));

let gl_Position = builtin(Vec4, "gl_Position");

let (=@) = (dest, src) => Assignment(dest, src);

let (+@) = (l, r) => Plus(l, r);

let (-@) = (l, r) => Minus(l, r);

let ( *@ ) = (l, r) => Mul(l, r);

let (/@) = (l, r) => Div(l, r);

/* One specific shader */
let position = attr(Vec4, "a_position");

let main =
  gfun(Void, "main", () =>
    /* gl_Position is a special variable a vertex shader is responsible for setting */ [
      Swizzle(gl_Position, XYZW)
      =@ Common(Swizzle(position, X))
      +@ Common(Swizzle(position, Y))
    ]
  );

let getShader = main =>
  version
  ++ newline
  ++ formatAttributes(getAttributes(main))
  ++ newline
  ++ newline
  ++ fmtFun(main);

let shader = getShader(main);
