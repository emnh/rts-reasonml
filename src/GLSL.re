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
      | Plus(l, r) => ["(", t.rExpr(t, l), " + ", t.rExpr(t, r), ")"]
      | Minus(l, r) => ["(", t.rExpr(t, l), " - ", t.rExpr(t, r), ")"]
      | Mul(l, r) => ["(", t.rExpr(t, l), " * ", t.rExpr(t, r), ")"]
      | Div(l, r) => ["(", t.rExpr(t, l), " / ", t.rExpr(t, r), ")"]
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


let formatAttributes = attrs =>
  MyString.join(newline, SS.elements(attrs));

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
