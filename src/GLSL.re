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

type leftExprT = varExprT;

type rightExprT =
  | Var(varExprT)
  | Plus(rightExprT, rightExprT);

type statementT =
  | Assignment(leftExprT, rightExprT)
  | Discard;

type gRootT = list(statementT);

type nodeT =
  | Statement(statementT)
  | LExpr(leftExprT)
  | RExpr(rightExprT);

let glslTypeString = t =>
  switch t {
  | Void => "void"
  | Int => "int"
  | Float => "float"
  | Vec2 => "vec2"
  | Vec3 => "vec3"
  | Vec4 => "vec4"
  };

let walkTree = (tree, f) =>
  List.iter(
    stmt =>
      /* f(Statement(stmt)); */
      switch stmt {
      | Assignment(left, right) =>
        f(LExpr(left));
        f(RExpr(right));
        ();
      | Discard =>
        f(Statement(Discard));
        ();
      },
    tree
  );

let rec fmtRExpr = rexpr =>
  switch rexpr {
  | Var(_, _, name) => name
  | Plus(l, r) => fmtRExpr(l) ++ " + " ++ fmtRExpr(r)
  };

let fmtLExpr = lexpr =>
  fmtRExpr(lexpr);

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

let attr = (t, name) => Var(Attribute, t, name);

let builtin = (t, name) => Var(Builtin, t, name);

let gfun = (t, name, ast) => (t, name, ast);

let getAttributes = gf => {
  let (_, _, astf) = gf;
  let tree = astf();
  let ar = ref([]);
  let f = x =>
    switch x {
    | LExpr(Var(Attribute, t, name)) =>
      ar := [(t, name), ...ar^];
      ();
    | RExpr(Var(Attribute, t, name)) =>
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

/* One specific shader */
let position = attr(Vec4, "a_position");

let main =
  gfun(Void, "main", () =>
    /* gl_Position is a special variable a vertex shader is responsible for setting */ [
      gl_Position =@ position
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
