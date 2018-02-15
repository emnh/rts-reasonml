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

type leftExprT =
  | Var(varExprT)
  | Swizzle(varExprT, swizzleT);

/* Translated from https://github.com/kovasb/gamma/blob/master/src/gamma/ast.cljs */
type rT =
  /* | TypeError */
  | RVar(varExprT)
  | RSwizzle(rT, swizzleT)
  | ImmediateFloat(float)
  | ImmediateInt(int)
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
  | Xor(rT, rT)
  | BuiltinFun(string, list(rT))
  | BuiltinFun1(string, rT)
  | BuiltinFun2(string, rT, rT)
  | BuiltinFun3(string, rT, rT, rT)
  | BuiltinFun4(string, rT, rT, rT, rT);

type rightExprT = rT;

type statementT =
  | Assignment(leftExprT, rightExprT)
  | Discard;

type gRootT = list(statementT);

type nodeT =
  | StatementNode(statementT)
  | LExprNode(leftExprT)
  | RExprNode(rightExprT)
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
  var: (transformer('a), varExprT) => 'a,
  combine: (transformer('a), list('a)) => 'a,
  rexprCombine: (transformer('a), list('a)) => 'a,
  lExpr: (transformer('a), leftExprT) => 'a,
  rExpr: (transformer('a), rightExprT) => 'a,
  tree: (transformer('a), list(statementT)) => 'a,
  tfun: (transformer('a), (glslTypeT, string, unit => list(statementT))) => 'a
};

let combine = (_, l) => List.fold_left((++), "", l);

let fmtTransformer = {
  var: (_, expr) => {
    let (_, _, name) = expr;
    name;
  },
  combine,
  rexprCombine: (t, l) => t.combine(t, ["(", t.combine(t, l), ")"]),
  lExpr: (t, expr) =>
    t.combine(
      t,
      switch expr {
      | Var(x) => [t.var(t, x)]
      | Swizzle((_, _, name), swizzle) => [name, ".", fmtSwizzle(swizzle)]
      }
    ),
  rExpr: (t, expr) =>
    t.rexprCombine(
      t,
      switch expr {
      | RVar(x) => [t.var(t, x)]
      | RSwizzle(expr, swizzle) => [t.rExpr(t, expr), ".", fmtSwizzle(swizzle)]
      | ImmediateFloat(e) => [string_of_float(e)]
      | ImmediateInt(e) => [string_of_int(e)]
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
      | BuiltinFun(name, l) =>
        let args = List.map(x => t.rExpr(t, x), l);
        List.concat([[name, "(", ...args], [")"]]);
      | BuiltinFun1(name, l) => [name, "(", t.rExpr(t, l), ")"]
      | BuiltinFun2(name, l, r) => [
          name,
          "(",
          t.rExpr(t, l),
          ", ",
          t.rExpr(t, r),
          ")"
        ]
      | BuiltinFun3(name, l, r, r2) => [
          name,
          "(",
          t.rExpr(t, l),
          ", ",
          t.rExpr(t, r),
          ", ",
          t.rExpr(t, r2),
          ")"
        ]
      | BuiltinFun4(name, l, r, r2, r3) => [
          name,
          "(",
          t.rExpr(t, l),
          ", ",
          t.rExpr(t, r),
          ", ",
          t.rExpr(t, r2),
          ", ",
          t.rExpr(t, r3),
          ")"
        ]
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

module SS = Set.Make(String);

let formatAttribute = attr => {
  let (t, name) = attr;
  "in " ++ glslTypeString(t) ++ " " ++ name ++ ";";
};

let getAttributes = gf => {
  let ar = ref(SS.empty);
  let walkTransformer = {
    ...fmtTransformer,
    var: (t, expr) => {
      switch expr {
      | (Attribute, t, name) => ar := SS.add(formatAttribute((t, name)), ar^)
      | _ => ()
      };
      fmtTransformer.var(t, expr);
    }
  };
  let _ = walkTransformer.tfun(walkTransformer, gf);
  ar^;
};

let formatAttributes = attrs => String.concat(newline, SS.elements(attrs));

let rightToLeft = right =>
  switch right {
  | RVar(x) => Var(x)
  | RSwizzle(RVar(x), s) => Swizzle(x, s)
  | _ => raise(GLSLTypeError("invalid left hand side"))
  };

let assign = (dest, src) => Assignment(rightToLeft(dest), src);

let symCounter = ref(0);

let genSym = () => {
  symCounter := symCounter^ + 1;
  "v_" ++ string_of_int(symCounter^);
};

let wrap = x => x;

let attr = (t, name) => wrap(RVar((Attribute, t, name)));

let builtin = (t, name) => wrap(RVar((Builtin, t, name)));

let uniform = (t, name) => wrap(RVar((Uniform, t, name)));

let var = (t, name) => wrap(RVar((Variable, t, name)));

let gfun = (t, name, ast) => (t, name, ast);

let floatattr = name => attr(Float, name);

let vec2attr = name => attr(Vec2, name);

let vec3attr = name => attr(Vec3, name);

let vec4attr = name => attr(Vec4, name);

let floatuniform = name => uniform(Float, name);

let vec2uniform = name => uniform(Vec2, name);

let vec3uniform = name => uniform(Vec3, name);

let vec4uniform = name => uniform(Vec4, name);

let floatvar = name => var(Float, name);

let vec2var = name => var(Vec2, name);

let vec3var = name => var(Vec3, name);

let vec4var = name => var(Vec4, name);

let gl_Position = builtin(Vec4, "gl_Position");

let gl_FragCoord = builtin(Vec4, "gl_FragCoord");

let gl_FragColor = builtin(Vec4, "gl_FragColor");

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
    let (++) = l => Inc(l);
    let (--) = l => Dec(l);
    let (+++) = l => PreInc(l);
    let (---) = l => PreDec(l);
    let (!) = l => Not(l);
    let ( * ) = (l, r) => Mul(l, r);
    let ( *= ) = (l, r) => l =@ l * r;
    let (/) = (l, r) => Div(l, r);
    let (/=) = (l, r) => l =@ l / r;
    let (+) = (l, r) => Plus(l, r);
    let (+=) = (l, r) => l =@ l + r;
    let (-) = (l, r) => Minus(l, r);
    let (-=) = (l, r) => l =@ l - r;
    let (<) = (l, r) => LessThan(l, r);
    let (>) = (l, r) => GreaterThan(l, r);
    let (<=) = (l, r) => LessThanOrEqual(l, r);
    let (>=) = (l, r) => GreaterThanOrEqual(l, r);
    let (==) = (l, r) => Equal(l, r);
    let (!=) = (l, r) => NotEqual(l, r);
    let (&&) = (l, r) => And(l, r);
    let (||) = (l, r) => Or(l, r);
    let (^^) = (l, r) => Xor(l, r);
    let ( **. ) = (var, st) => RSwizzle(var, st);
    let sin = l => BuiltinFun1("sin", l);
    let cos = l => BuiltinFun1("cos", l);
    let vec3 = l => BuiltinFun("vec3", l);
    let vec4 = l => BuiltinFun("vec4", l);
    let f = x => ImmediateFloat(x);
    let i = x => ImmediateInt(x);
  };
};

module VertexShader = Fragment.Make(Fragment.ElementModule);

module FragmentShader = VertexShader;

let main =
  gfun(
    Void,
    "main",
    () => {
      open! VertexShader;
      /* gl_Position is a special variable a vertex shader is responsible for setting */
      gl_Position **. XYZW =@ position4 **. X + position4 **. Y;
      gl_Position **. XYZW =@ position3 **. XYZ + position3 **. XYZ;
      gl_Position **. XYZW =@ position4 **. XYZW + position4 **. XYZW;
      gl_Position =@ position4 **. XYZW + position4 **. XYZW;
      finish();
    }
  );

let resolution = vec2uniform("resolution");

let time = floatuniform("time");

let mainFrag =
  gfun(
    Void,
    "main",
    () => {
      open! FragmentShader;
      let position = vec2var("position");
      position =@ gl_FragCoord **. XY / (resolution **. XY);
      let color = floatvar("color");
      color =@ f(0.0);
      color
      += (
        sin(position **. X * cos(time / f(15.0)) * f(80.0))
        + cos(position **. Y * cos(time / f(15.0)) * f(10.0))
      );
      color
      += (
        sin(position **. Y * sin(time / f(10.0)) * f(40.0))
        + cos(position **. X * sin(time / f(25.0)) * f(40.0))
      );
      color
      += (
        sin(position **. X * sin(time / f(5.0)) * f(10.0))
        + sin(position **. Y * sin(time / f(35.0)) * f(80.0))
      );
      color *= (sin(time / f(10.0)) * f(0.5));
      gl_FragColor
      =@ vec4([
           vec3([color, color * f(0.5), sin(color + time / f(3.0)) * f(0.75)]),
           f(1.0)
         ]);
      gl_FragColor =@ position4 **. XYZW;
      finish();
    }
  );

let getShader = main =>
  version
  ++ newline
  ++ newline
  ++ formatAttributes(getAttributes(main))
  ++ newline
  ++ newline
  ++ fmtFun(main);

/* let shader = getShader(main); */
let shader = getShader(mainFrag);
