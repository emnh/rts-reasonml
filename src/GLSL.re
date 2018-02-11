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

type rightExprT =
  /* | TypeError */
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

/* let rec walkCommonExpr = (tree, combine, transform) =>
     switch tree {
     | Var(e) => combine([transform(VarNode(e))])
     | Swizzle(tree, st) =>
       combine([transform(CommonExprNode(tree)), transform(SwizzleNode(st))]);
       walkCommonExpr(tree, st);
     };

   let walkLExpr = (tree, combine, transform) =>
     walkCommonExpr(tree, combine, transform);

   let rec walkRExpr = (tree, combine, transform) =>
     switch tree {
     | Common(expr) =>
       combine([transform(CommonExprNode(expr))]);
       walkCommonExpr(expr, combine, transform);
     | Plus(l, r) =>
       combine([transform(RExprNode(l)), transform(RExprNode(r))]);
       walkRExpr(l, combine, transform);
       walkRExpr(r, combine, transform);
     | Minus(l, r) =>
       combine([f(RExprNode(l)), f(RExprNode(r))]);
       walkRExpr(l, combine, transform);
       walkRExpr(r, combine, transform);
     | Mul(l, r) =>
       combine([f(RExprNode(l)), f(RExprNode(r))]);
       walkRExpr(l, combine, transform);
       walkRExpr(r, combine, transform);
     | Div(l, r) =>
       combine([f(RExprNode(l)), f(RExprNode(r))]);
       walkRExpr(l, combine, transform);
       walkRExpr(r, combine, transform);
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
     ); */
/* let fmtTransform = t => t;

   let fmtCombine = l => List.reduce((++), l);

   let fmtNew = tree => walkTree(tree, fmtCombine, fmtTransform); */
/* module Transformer = {
     let plusLeft =
     let combinePlus = (l, r) => ;
   }; */
type transformer('a) = {
  .
  combine: list('a) => 'a,
  rExpr: rightExprT => 'a,
  plusCombine: list('a) => 'a,
  plusLeft: rightExprT => 'a,
  plusRight: rightExprT => 'a
};

let fmtTransformer: transformer(list(string)) = {
  combine: l => List.reduce((++), l),
  rExpr: expr =>
    switch rexpr {
    | Common(expr) => fmtCommon(expr)
    | Plus(l, r) => ["(", this#plusLeft(l), " + ", this#plusRight(r), ")"]
    | Minus(l, r) => ["(", fmtRExpr(l), " - ", fmtRExpr(r), ")"]
    | Mul(l, r) => ["(", fmtRExpr(l), " * ", fmtRExpr(r), ")"]
    | Div(l, r) => ["(", fmtRExpr(l), " / ", fmtRExpr(r), ")"]
    },
  plusCombine: this#combine(expr),
  plusLeft: expr => this#rExpr(expr),
  plusRight: expr => this#rExpr(expr)
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

let (=@) = (dest, src) => {
  let dest =
    switch dest {
    | Common(x) => x
    | _ => raise(GLSLTypeError("left hand side must be common expression"))
    };
  Assignment(dest, src);
};

let (+@) = (l, r) => Plus(l, r);

let (-@) = (l, r) => Minus(l, r);

let ( *@ ) = (l, r) => Mul(l, r);

let (/@) = (l, r) => Div(l, r);

let ( **. ) = (var, st) => Common(Swizzle(var, st));

let symCounter = ref(0);

let genSym = () => {
  symCounter := symCounter^ + 1;
  "v_" ++ string_of_int(symCounter^);
};

module type MVarType = {let varType: varT; let name: option(string);};

module type MBaseType =
  (MVarType) =>
  {
    let get: unit => commonExprT;
    let getVarType: unit => varT;
    let getName: unit => string;
  };

module MBase: MBaseType =
  (MVar: MVarType) => {
    let varType = MVar.varType;
    let name =
      switch MVar.name {
      | Some(x) => x
      | None => genSym()
      };
    let get = () => Var((varType, Vec4, name));
    let getVarType = () => varType;
    let getName = () => name;
    /* let cget = () => Common(get()); */
  };

module type MVec4Type =
  (MVarType) =>
  {
    let x: unit => rightExprT;
    let y: unit => rightExprT;
    let z: unit => rightExprT;
    let w: unit => rightExprT;
    let xyzw: unit => rightExprT;
  };

module MVec4: MVec4Type =
  (MVar: MVarType) => {
    include MBase(MVar);
    let x = () => Common(Swizzle(get(), X));
    let y = () => Common(Swizzle(get(), Y));
    let z = () => Common(Swizzle(get(), Z));
    let w = () => Common(Swizzle(get(), W));
    let xyzw = () => Common(Swizzle(get(), XYZW));
  };

module GL_PositionV: MVarType = {
  let name = Some("gl_Position");
  let varType = Builtin;
};

module GL_Position = MVec4(GL_PositionV);

/* One specific shader */
module PositionV: MVarType = {
  let name = Some("a_position");
  let varType = Attribute;
};

module Position = MVec4(PositionV);

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

let vec3attr = name => {
  let varExpr = (Attribute, Vec4, name);
  {
    varExpr,
    self: Common(Var(varExpr)),
    x: Common(Swizzle(Var(varExpr), X)),
    y: Common(Swizzle(Var(varExpr), Y)),
    xyz: Common(Swizzle(Var(varExpr), XYZ))
  };
};

let vec4attr = name => {
  let varExpr = (Attribute, Vec4, name);
  {
    varExpr,
    self: Common(Var(varExpr)),
    x: Common(Swizzle(Var(varExpr), X)),
    y: Common(Swizzle(Var(varExpr), Y)),
    xyz: Common(Swizzle(Var(varExpr), XYZ)),
    xyzw: Common(Swizzle(Var(varExpr), XYZW))
  };
};

let position = attr(Vec4, "a_position");

let fposition3 = vec3attr("a_position");

let fposition4 = vec4attr("a_position");

let main =
  gfun(Void, "main", () =>
    /* gl_Position is a special variable a vertex shader is responsible for setting */ [
      gl_Position **. XYZW =@ position **. X +@ position **. Y,
      GL_Position.xyzw() =@ Position.x() +@ Position.y(),
      GL_Position.xyzw() =@ fposition4.x +@ fposition4.y,
      GL_Position.xyzw() =@ fposition3.xyz +@ fposition3.xyz,
      GL_Position.xyzw() =@ fposition4.xyzw +@ fposition4.xyzw
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
