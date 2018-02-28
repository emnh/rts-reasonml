module SS = Set.Make(String);

let version = "#version 300 es";

let vertexPrelude = {|
#define VARYING out

|};

let fragmentPrelude = {|
#define VARYING in|};

let precisionQuantifier = "mediump";

let precision = "precision " ++ precisionQuantifier ++ " float;";

let newline = "\n";

let indent = "  ";

exception GLSLTypeError(string);

type glslTypeT =
  | Void
  | Int
  | UInt
  | Float
  | Vec2
  | Vec3
  | Vec4
  | Mat2
  | Mat3
  | Mat4
  | Sampler2D
  | SamplerCube;

type varT =
  | Builtin
  | Attr
  | Uniform
  | Varying
  | Var
  | Output
  | Arg
  | Fun;

type varExprT = (varT, glslTypeT, string);

include GLSLSwizzleFormat;

type glslVariantTypeT =
  [
    | `Void
    | `Bool
    | `Int
    | `UInt
    | `Float
    | `Vec2
    | `Vec3
    | `Vec4
    | `Mat2
    | `Mat3
    | `Mat4
    | `Sampler2D
  ] as 'a;

type glslVariantTypeUBT('a) = [< glslVariantTypeT] as 'a;

type glslVariantTypeLBT('a) = [> glslVariantTypeT] as 'a;

type nodeType('a, 'b) = 'a => 'b
constraint 'a = glslVariantTypeUBT('a)
constraint 'b = glslVariantTypeLBT('b);

type leftExprT =
  | Var(varExprT)
  | Swizzle(varExprT, swizzleT)
and lT = leftExprT
/* list(rT) in funExprT is too wide,
 * but it makes it easy to use vars as arguments and
 * as right expressions interchangeably */
and funExprT = (glslTypeT, string, list(rT), gRootT)
/* rT: Translated from https://github.com/kovasb/gamma/blob/master/src/gamma/ast.cljs */
and rT =
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
  /* | PlusMinus(rT('a,'b)) */
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
  | Ternary(rT, rT, rT)
  | Concat(rT, rT)
  | BuiltinFun(string, list(rT))
  | BuiltinFun1(string, rT)
  | BuiltinFun2(string, rT, rT)
  | BuiltinFun3(string, rT, rT, rT)
  | BuiltinFun4(string, rT, rT, rT, rT)
  | CustomFun(funExprT, list(rT))
and grT('a) = rT
and rightExprT = rT
and trT('a) =
  | Typed('a, rT)
  | Untyped(rT)
and statementT =
  | Assignment(lT, rT)
  | DeclAssignment(glslTypeT, lT, rT)
  | ForStatement(gRootT, rT, gRootT, gRootT)
  | IfStatement(rT, gRootT)
  | IfElseStatement(rT, gRootT, gRootT)
  | Return(rT)
  | ReturnVoid
  | Discard
and gRootT = list(statementT);

/*
 type cfun1 = 'a => trT('b);

 type cfun2('a, 'b, 'c) =  => trT('c);
 */
let unused = x => x;

let u = te =>
  switch te {
  | Typed(_, e) => e
  | Untyped(e) => e
  };

let untyped = u;

let getType = x =>
  switch x {
  | Typed(t, _) => t
  | _ => raise(GLSLTypeError("type mismatch"))
  };

type t('a, 'b) = PhantomAlgebra.t('a, 'b);

type genType1T('a, 'c) = [< | `zero('a) | `one('a)] as 'c;

type genType2T('a, 'b, 'c) = [< | `zero('b) &('a) | `one('b) &('a)] as 'c
constraint 'a = 'b;

let genericexprlist:
  (list(trT(t('dim, genType1T('rank1, 'rank3)))), 'a) => trT(t('dim, 'rank3)) =
  (_, e) => Typed(protoScalar, unused(e));

let genericexpr1:
  (trT(t('dim, genType1T('rank1, 'rank2))), 'a) => trT(t('dim, 'rank2)) =
  (l, e) => Typed(getType(l), unused(e));

let genericexpr1float: (trT(t('dim, genType1T('rank1, 'rank2))), 'a) => trT('b) =
  (_, e) => Typed(protoScalar, unused(e));

let genericexpr2:
  (trT(t('dim, genType1T('rank1, 'rank2))), trT(t('dim, 'rank2)), 'a) =>
  trT(t('dim, 'rank2)) =
  (l, _, e) => Typed(getType(l), e);

let genericexpr2float:
  (trT(t('dim, genType1T('rank1, 'rank2))), trT(t('dim, 'rank2)), 'a) =>
  trT('b) =
  (_, _, e) => Typed(PhantomAlgebra.scalar(0.0), e);

/* TODO: constrain rank3 to be zero or equal to rank2 */
let mixexpr3:
  (
    trT(t('dim, genType1T('rank1, 'rank2))),
    trT(t('dim, 'rank2)),
    trT(t('dim2, genType1T('rank3, 'rank4))),
    'a
  ) =>
  trT(t('dim, 'rank2)) =
  (l, _, _, e) => Typed(getType(l), e);

let oldgenericexprlist = (l, e) =>
  switch (List.hd(l)) {
  | Typed(`Float, _) => Typed(`Float, unused(e))
  | Typed(`Vec2, _) => Typed(`Vec2, unused(e))
  | Typed(`Vec3, _) => Typed(`Vec3, unused(e))
  | Typed(`Vec4, _) => Typed(`Vec4, unused(e))
  | Untyped(_) => raise(GLSLTypeError("type mismatch"))
  };

let oldgenericexpr1 = (l, e) =>
  switch l {
  | Typed(`Float, _) => Typed(`Float, unused(e))
  | Typed(`Vec2, _) => Typed(`Vec2, unused(e))
  | Typed(`Vec3, _) => Typed(`Vec3, unused(e))
  | Typed(`Vec4, _) => Typed(`Vec4, unused(e))
  | Untyped(_) => raise(GLSLTypeError("type mismatch"))
  };

let oldgenericexpr1float = (l, e) =>
  switch l {
  | Typed(`Float, _) => Typed(`Float, unused(e))
  | Typed(`Vec2, _) => Typed(`Float, unused(e))
  | Typed(`Vec3, _) => Typed(`Float, unused(e))
  | Typed(`Vec4, _) => Typed(`Float, unused(e))
  | Untyped(_) => raise(GLSLTypeError("type mismatch"))
  };

type gm2T('a) =
  [< | `Float | `Vec2 | `Vec3 | `Vec4 | `Mat2 | `Mat3 | `Mat4] as 'a;

type gm2fT('a, 'b, 'c) = (trT('a), trT('b), 'c) => trT('a);

/*
 constraint 'a = gm2T('a)
 constraint 'b = gm2T('b);
 */
/*let genericmath2: gm2fT('a, 'b, 'c) =*/
let oldgenericmath2 = (l, r: trT(glslVariantTypeUBT('a)), e) =>
  switch (l, r) {
  | (Typed(_ as a, _), Typed(`Float, _)) => Typed(a, unused(e))
  | (Typed(_ as a, _), Typed(_, _)) => Typed(a, unused(e))
  | _ => raise(GLSLTypeError("type mismatch"))
  };

/*
 switch (l, r) {
 | (Typed(`Float as a, _), Typed(`Float, _)) => Typed(a, unused(e))
 | (Typed(`Vec2 as a, _), Typed(`Vec2, _)) => Typed(a, unused(e))
 | (Typed(`Vec3 as a, _), Typed(`Vec3, _)) => Typed(a, unused(e))
 | (Typed(`Vec4 as a, _), Typed(`Vec4, _)) => Typed(a, unused(e))
 | (Typed(`Mat2 as a, _), Typed(`Mat2, _)) => Typed(a, unused(e))
 | (Typed(`Mat3 as a, _), Typed(`Mat3, _)) => Typed(a, unused(e))
 | (Typed(`Mat4 as a, _), Typed(`Mat4, _)) => Typed(a, unused(e))
 | _ => raise(GLSLTypeError("type mismatch"))
 };
 */
type genTypeUBT('a) = [< | `Float | `Vec2 | `Vec3 | `Vec4] as 'a;

type genTypeT('a) = trT('a) constraint 'a = genTypeUBT('a);

/* TODO: Only accept `Float to `Vec4 */
let oldgenericexpr2: (genTypeT('a), genTypeT('a), 'b) => genTypeT('a) =
  (l, r, e) =>
    switch (l, r) {
    | (Typed(_ as a, _), Typed(_, _)) => Typed(a, unused(e))
    | _ => raise(GLSLTypeError("type mismatch"))
    };

/* TODO: Only accept `Float to `Vec4 */
let oldgenericexpr2float: ('a, 'a, 'b) => 'c =
  (l, r, e) =>
    switch (l, r) {
    | (Typed(_, _), Typed(_, _)) => Typed(`Float, unused(e))
    | _ => raise(GLSLTypeError("type mismatch"))
    };

/*
 type nodeT =
   | StatementNode(statementT)
   | LExprNode(leftExprT)
   | RExprNode(rightExprT)
   | VarNode(varExprT)
   | SwizzleNode(swizzleT);
   */
let glslTypeString = t =>
  switch t {
  | Void => "void"
  | Int => "int"
  | UInt => "uint"
  | Float => "float"
  | Vec2 => "vec2"
  | Vec3 => "vec3"
  | Vec4 => "vec4"
  | Mat2 => "mat2"
  | Mat3 => "mat3"
  | Mat4 => "mat4"
  | Sampler2D => "sampler2D"
  | SamplerCube => "samplerCube"
  };

type shaderMainT = gRootT;

/* TODO: should probably be an object */
type transformer('a) = {
  var: (transformer('a), varExprT) => 'a,
  combine: (transformer('a), list('a)) => 'a,
  rexprCombine: (transformer('a), list('a)) => 'a,
  lExpr: (transformer('a), lT) => 'a,
  rExpr: (transformer('a), rT) => 'a,
  tree: (transformer('a), list(statementT)) => 'a,
  tfun: (transformer('a), shaderMainT) => 'a
};

let combine = (_, l) => List.fold_left((++), "", l);

let fmtTransformer = {
  var: (_, expr) => {
    let (_, _, name) = expr;
    name;
  },
  combine,
  /* rexprCombine: (t, l) => t.combine(t, ["(", t.combine(t, l), ")"]), */
  rexprCombine: (t, l) => t.combine(t, [t.combine(t, l)]),
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
      | Ternary(l, r1, r2) => [
          "(",
          t.rExpr(t, l),
          " ? ",
          t.rExpr(t, r1),
          " : ",
          t.rExpr(t, r2),
          ")"
        ]
      | Concat(l, r) => [t.rExpr(t, l), ", ", t.rExpr(t, r)]
      | CustomFun((_, name, _, _), l) =>
        let args = List.map(x => t.rExpr(t, x), l);
        let args =
          List.fold_left(
            (x, y) => List.concat([x, [",", y]]),
            [List.hd(args)],
            List.tl(args)
          );
        List.concat([[name, "("], args, [")"]]);
      | BuiltinFun(name, l) =>
        let args = List.map(x => t.rExpr(t, x), l);
        let args =
          List.fold_left(
            (x, y) => List.concat([x, [",", y]]),
            [List.hd(args)],
            List.tl(args)
          );
        List.concat([[name, "("], args, [")"]]);
      | BuiltinFun1(name, l) => [name, "(", t.rExpr(t, l), ")"]
      | BuiltinFun2(name, l, r) => [
          name,
          "(",
          t.rExpr(t, l),
          ", ",
          t.rExpr(t, r),
          ")"
        ]
      /*| Texture(l, r) => [
          "texture",
          "(",
          t.rExpr(t, l),
          ", ",
          t.rExpr(t, r),
          ")"
        ]*/
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
              | DeclAssignment(vart, left, right) =>
                t.combine(
                  t,
                  [
                    glslTypeString(vart),
                    " ",
                    t.lExpr(t, left),
                    " = ",
                    t.rExpr(t, right)
                  ]
                )
              | Return(right) =>
                t.combine(t, ["return ", t.rExpr(t, right), ";"])
              | ReturnVoid => t.combine(t, ["return;"])
              | ForStatement(l, test, r2, body) =>
                let body = t.tree(t, body);
                t.combine(
                  t,
                  [
                    "for (",
                    /* TODO: handle multiple init statements */
                    String.map(
                      x =>
                        switch x {
                        | ';' => ' '
                        | _ => x
                        },
                      t.tree(t, l)
                    ),
                    ";",
                    t.rExpr(t, test),
                    ";",
                    /* TODO: handle multiple increment statements */
                    String.map(
                      x =>
                        switch x {
                        | ';' => ' '
                        | _ => x
                        },
                      t.tree(t, r2)
                    ),
                    ") {",
                    newline,
                    body,
                    newline,
                    "}"
                  ]
                );
              | IfStatement(l, body) =>
                let body = t.tree(t, body);
                t.combine(
                  t,
                  ["if (", t.rExpr(t, l), ") {", newline, body, newline, "}"]
                );
              | IfElseStatement(l, b1, b2) =>
                let b1 = t.tree(t, b1);
                let b2 = t.tree(t, b2);
                t.combine(
                  t,
                  [
                    "if (",
                    t.rExpr(t, l),
                    ") {",
                    newline,
                    b1,
                    newline,
                    "} else {",
                    newline,
                    b2,
                    newline,
                    "}"
                  ]
                );
              | Discard => "discard"
              },
              ";",
              newline
            ]
          ),
        tree
      )
    ),
  tfun: (t, tree) =>
    t.combine(t, ["void main() {", newline, t.tree(t, tree), "}"])
};

let fmtFun = gf => fmtTransformer.tfun(fmtTransformer, gf);

let orderVars = (fmt, vars) =>
  List.map(fmt, List.sort_uniq(Pervasives.compare, vars));

let getVarOfType = (vart, gf) => {
  let ar = ref([]);
  let walkTransformer = {
    ...fmtTransformer,
    var: (t, expr) => {
      switch expr {
      | (x, t, name) when x == vart => ar := [(t, name), ...ar^]
      | _ => ()
      };
      fmtTransformer.var(t, expr);
    },
    rExpr: (t, expr) => {
      switch expr {
      | CustomFun((_, _, _, body), _) =>
        let _ = t.tree(t, body);
        ();
      | _ => ()
      };
      fmtTransformer.rExpr(t, expr);
    }
  };
  let _ = walkTransformer.tfun(walkTransformer, gf);
  ar^;
};

let getFunctions = gf => {
  let ar = ref([]);
  let walkTransformer = {
    ...fmtTransformer,
    rExpr: (t, expr) => {
      switch expr {
      | CustomFun((a, b, c, body), _) =>
        ar := [(a, b, c, body), ...ar^];
        let _ = t.tree(t, body);
        ();
      | _ => ()
      };
      fmtTransformer.rExpr(t, expr);
    }
  };
  let _ = walkTransformer.tfun(walkTransformer, gf);
  ar^;
};

let getAttributes = gf => getVarOfType(Attr, gf);

let getVaryings = gf => getVarOfType(Varying, gf);

let getUniforms = gf => getVarOfType(Uniform, gf);

let getOutputs = gf => getVarOfType(Output, gf);

/*
 type fft = list((glslTypeT, SS.elt, list(trT('a,'b)), gRootT)) => string;

 type fftinner =
   ((glslTypeT, SS.elt, list(rT), list(statementT))) => SS.elt;
   */
let formatFunctions = attrs => {
  let t = fmtTransformer;
  let fns = ref(SS.empty);
  let formatFunction = attr => {
    let (vart, name, argvars, body) = attr;
    let argvars =
      List.map(
        x =>
          switch x {
          | RVar((_, vart, name)) =>
            t.combine(t, [glslTypeString(vart), " ", name])
          | _ => ""
          },
        argvars
      );
    let argvars =
      List.fold_left(
        (x, y) => List.concat([x, [",", y]]),
        [List.hd(argvars)],
        List.tl(argvars)
      );
    let body = t.tree(t, body);
    let fn =
      t.combine(
        t,
        List.concat([
          [glslTypeString(vart), " ", name, "("],
          argvars,
          [") {", newline, body, newline, "}"]
        ])
      );
    switch (SS.find(fn, fns^)) {
    | _ => ""
    | exception Not_found =>
      fns := SS.add(fn, fns^);
      fn;
    };
  };
  String.concat(newline, List.map(formatFunction, attrs));
};

let formatAttributes = attrs => {
  let formatAttribute = attr => {
    let (t, name) = attr;
    "in " ++ glslTypeString(t) ++ " " ++ name ++ ";";
  };
  String.concat(newline, orderVars(formatAttribute, attrs));
};

let formatVaryings = attrs => {
  let formatVarying = attr => {
    let (t, name) = attr;
    "VARYING " ++ glslTypeString(t) ++ " " ++ name ++ ";";
  };
  String.concat(newline, orderVars(formatVarying, attrs));
};

let formatUniforms = attrs => {
  let formatUniform = attr => {
    let (t, name) = attr;
    indent
    ++ precisionQuantifier
    ++ " "
    ++ glslTypeString(t)
    ++ " "
    ++ name
    ++ ";";
  };
  let formatUniform2 = attr => {
    let (t, name) = attr;
    "uniform " ++ glslTypeString(t) ++ " " ++ name ++ ";";
  };
  let filterSamplers = ((t, _)) =>
    switch t {
    | Sampler2D => false
    | SamplerCube => false
    | _ => true
    };
  let (notSamplerAttrs, samplerAttrs) = List.partition(filterSamplers, attrs);
  "layout(std140) uniform u_PerScene {"
  ++ newline
  ++ String.concat(newline, List.map(formatUniform, notSamplerAttrs))
  ++ newline
  ++ "};"
  ++ newline
  ++ String.concat(newline, List.map(formatUniform2, samplerAttrs))
  ++ newline;
};

let formatOutputs = attrs => {
  let formatAttribute = attr => {
    let (t, name) = attr;
    "out " ++ glslTypeString(t) ++ " " ++ name ++ ";";
  };
  String.concat(newline, orderVars(formatAttribute, attrs));
};

let getShader = (prelude, uniforms, varyings, main) =>
  version
  ++ newline
  ++ newline
  ++ prelude
  ++ newline
  ++ precision
  ++ formatAttributes(getAttributes(main))
  ++ newline
  ++ newline
  ++ formatVaryings(varyings)
  ++ newline
  ++ newline
  ++ formatUniforms(uniforms)
  ++ newline
  ++ newline
  ++ formatOutputs(getOutputs(main))
  ++ newline
  ++ newline
  ++ formatFunctions(getFunctions(main))
  ++ newline
  ++ newline
  ++ fmtFun(main);

type programT = {
  attributes: list((glslTypeT, string)),
  uniforms: list((glslTypeT, string)),
  varyings: list((glslTypeT, string)),
  outputs: list((glslTypeT, string)),
  vertexShader: string,
  fragmentShader: string
};

let rightToLeft = right =>
  switch right {
  | RVar(x) => (x, Var(x))
  | RSwizzle(RVar(x), s) => (x, Swizzle(x, s))
  | _ => raise(GLSLTypeError("invalid left hand side"))
  };

let getProgram = (uniformBlock, vsmain, fsmain) => {
  /*
   let uniforms = List.concat([getUniforms(vsmain), getUniforms(fsmain)]);
   */
  let uf = ((e, _)) => {
    let ((_, x, y), _) = rightToLeft(e);
    (x, y);
  };
  let uniforms = List.map(uf, uniformBlock);
  let varyings = List.concat([getVaryings(vsmain), getVaryings(fsmain)]);
  {
    attributes: getAttributes(vsmain),
    outputs: getOutputs(fsmain),
    uniforms,
    varyings,
    vertexShader: getShader(vertexPrelude, uniforms, varyings, vsmain),
    fragmentShader: getShader(fragmentPrelude, uniforms, varyings, fsmain)
  };
};

let assign = (hasVar, addVar, dest, src) => {
  let ((vart, glslt, name), left) = rightToLeft(dest);
  if (hasVar(name) || vart != Var) {
    Assignment(left, src);
  } else {
    if (vart == Var) {
      addVar(name);
    };
    DeclAssignment(glslt, left, src);
  };
};

let symCounter = ref(0);

let genSym = () => {
  symCounter := symCounter^ + 1;
  "v_" ++ string_of_int(symCounter^);
};

let wrap = (gt, x) =>
  /*
   let gt =
     switch x {
     | RVar((_, vart, _)) =>
       switch vart {
       | Void => `Void
       | Int => `Int
       | Float => `Float
       | Vec2 => `Vec2
       | Vec3 => `Vec3
       | Vec4 => `Vec4
       | Mat2 => `Mat2
       | Mat3 => `Mat3
       | Mat4 => `Mat4
       | Sampler2D => `Sampler2D
       }
     | _ => raise(GLSLTypeError("type mismatch"))
     };*/
  Typed(gt, x);

let unwrap = x => {
  let (t, e) =
    switch x {
    | Typed(vart, e) => (
        switch vart {
        | `Void => Void
        | `Int => Int
        | `Float => Float
        | `Vec2 => Vec2
        | `Vec3 => Vec3
        | `Vec4 => Vec4
        | `Mat2 => Mat2
        | `Mat3 => Mat3
        | `Mat4 => Mat4
        | `Sampler2D => Sampler2D
        },
        e
      )
    | Untyped(_) => raise(GLSLTypeError("untyped expression"))
    };
  Typed(t, e);
};

/*
 let fundecl = (_fun, argtypes, body, args) => {
   let (funt, adtfunt, name) =
     switch _fun {
     | Typed(variantt, RVar((Fun, vart, name))) => (variantt, vart, name)
     | _ => raise(GLSLTypeError("type mismatch"))
     };
   Typed(
     funt,
     CustomFun((adtfunt, name, List.map(u, argtypes), body), List.map(u, args))
   );
 };
 */
let fundecl1 = (_fun, argtype: trT('a), body, arg: trT('a)) => {
  let (funt, adtfunt, name) =
    switch _fun {
    | Typed(variantt, RVar((Fun, vart, name))) => (variantt, vart, name)
    | _ => raise(GLSLTypeError("type mismatch"))
    };
  Typed(funt, CustomFun((adtfunt, name, [u(argtype)], body), [u(arg)]));
};

let fundecl2 =
    (
      _fun,
      (argtype1: trT('a), argtype2: trT('b)),
      body,
      arg1: trT('a),
      arg2: trT('b)
    ) => {
  let (funt, adtfunt, name) =
    switch _fun {
    | Typed(variantt, RVar((Fun, vart, name))) => (variantt, vart, name)
    | _ => raise(GLSLTypeError("type mismatch"))
    };
  Typed(
    funt,
    CustomFun(
      (adtfunt, name, [u(argtype1), u(argtype2)], body),
      [u(arg1), u(arg2)]
    )
  );
};

let fundecl3 =
    (
      _fun,
      (argtype1: trT('a), argtype2: trT('b), argtype3: trT('c)),
      body,
      arg1: trT('a),
      arg2: trT('b),
      arg3: trT('c)
    ) => {
  let (funt, adtfunt, name) =
    switch _fun {
    | Typed(variantt, RVar((Fun, vart, name))) => (variantt, vart, name)
    | _ => raise(GLSLTypeError("type mismatch"))
    };
  Typed(
    funt,
    CustomFun(
      (adtfunt, name, [u(argtype1), u(argtype2), u(argtype3)], body),
      [u(arg1), u(arg2), u(arg3)]
    )
  );
};

let fundecl4 =
    (
      _fun,
      (
        argtype1: trT('a),
        argtype2: trT('b),
        argtype3: trT('c),
        argtype4: trT('d)
      ),
      body,
      arg1: trT('a),
      arg2: trT('b),
      arg3: trT('c),
      arg4: trT('d)
    ) => {
  let (funt, adtfunt, name) =
    switch _fun {
    | Typed(variantt, RVar((Fun, vart, name))) => (variantt, vart, name)
    | _ => raise(GLSLTypeError("type mismatch"))
    };
  Typed(
    funt,
    CustomFun(
      (
        adtfunt,
        name,
        [u(argtype1), u(argtype2), u(argtype3), u(argtype4)],
        body
      ),
      [u(arg1), u(arg2), u(arg3), u(arg4)]
    )
  );
};

/* Begin auto-generated by GLSLFunGen.py */
let floatfun = name => Typed(protoFloat, RVar((Fun, Float, name)));

let floatattr = name => Typed(protoFloat, RVar((Attr, Float, name)));

let floatvarying = name => Typed(protoFloat, RVar((Varying, Float, name)));

let floatbuiltin = name => Typed(protoFloat, RVar((Builtin, Float, name)));

let floatuniform = name => Typed(protoFloat, RVar((Uniform, Float, name)));

let floatvar = name => Typed(protoFloat, RVar((Var, Float, name)));

let floatoutput = name => Typed(protoFloat, RVar((Output, Float, name)));

let floatarg = name => Typed(protoFloat, RVar((Arg, Float, name)));

let vec2fun = name => Typed(protoVec2, RVar((Fun, Vec2, name)));

let vec2attr = name => Typed(protoVec2, RVar((Attr, Vec2, name)));

let vec2varying = name => Typed(protoVec2, RVar((Varying, Vec2, name)));

let vec2builtin = name => Typed(protoVec2, RVar((Builtin, Vec2, name)));

let vec2uniform = name => Typed(protoVec2, RVar((Uniform, Vec2, name)));

let vec2var = name => Typed(protoVec2, RVar((Var, Vec2, name)));

let vec2output = name => Typed(protoVec2, RVar((Output, Vec2, name)));

let vec2arg = name => Typed(protoVec2, RVar((Arg, Vec2, name)));

let vec3fun = name => Typed(protoVec3, RVar((Fun, Vec3, name)));

let vec3attr = name => Typed(protoVec3, RVar((Attr, Vec3, name)));

let vec3varying = name => Typed(protoVec3, RVar((Varying, Vec3, name)));

let vec3builtin = name => Typed(protoVec3, RVar((Builtin, Vec3, name)));

let vec3uniform = name => Typed(protoVec3, RVar((Uniform, Vec3, name)));

let vec3var = name => Typed(protoVec3, RVar((Var, Vec3, name)));

let vec3output = name => Typed(protoVec3, RVar((Output, Vec3, name)));

let vec3arg = name => Typed(protoVec3, RVar((Arg, Vec3, name)));

let vec4fun = name => Typed(protoVec4, RVar((Fun, Vec4, name)));

let vec4attr = name => Typed(protoVec4, RVar((Attr, Vec4, name)));

let vec4varying = name => Typed(protoVec4, RVar((Varying, Vec4, name)));

let vec4builtin = name => Typed(protoVec4, RVar((Builtin, Vec4, name)));

let vec4uniform = name => Typed(protoVec4, RVar((Uniform, Vec4, name)));

let vec4var = name => Typed(protoVec4, RVar((Var, Vec4, name)));

let vec4output = name => Typed(protoVec4, RVar((Output, Vec4, name)));

let vec4arg = name => Typed(protoVec4, RVar((Arg, Vec4, name)));

let mat2fun = name => Typed(protoMat2, RVar((Fun, Mat2, name)));

let mat2attr = name => Typed(protoMat2, RVar((Attr, Mat2, name)));

let mat2varying = name => Typed(protoMat2, RVar((Varying, Mat2, name)));

let mat2builtin = name => Typed(protoMat2, RVar((Builtin, Mat2, name)));

let mat2uniform = name => Typed(protoMat2, RVar((Uniform, Mat2, name)));

let mat2var = name => Typed(protoMat2, RVar((Var, Mat2, name)));

let mat2output = name => Typed(protoMat2, RVar((Output, Mat2, name)));

let mat2arg = name => Typed(protoMat2, RVar((Arg, Mat2, name)));

let mat3fun = name => Typed(protoMat3, RVar((Fun, Mat3, name)));

let mat3attr = name => Typed(protoMat3, RVar((Attr, Mat3, name)));

let mat3varying = name => Typed(protoMat3, RVar((Varying, Mat3, name)));

let mat3builtin = name => Typed(protoMat3, RVar((Builtin, Mat3, name)));

let mat3uniform = name => Typed(protoMat3, RVar((Uniform, Mat3, name)));

let mat3var = name => Typed(protoMat3, RVar((Var, Mat3, name)));

let mat3output = name => Typed(protoMat3, RVar((Output, Mat3, name)));

let mat3arg = name => Typed(protoMat3, RVar((Arg, Mat3, name)));

let mat4fun = name => Typed(protoMat4, RVar((Fun, Mat4, name)));

let mat4attr = name => Typed(protoMat4, RVar((Attr, Mat4, name)));

let mat4varying = name => Typed(protoMat4, RVar((Varying, Mat4, name)));

let mat4builtin = name => Typed(protoMat4, RVar((Builtin, Mat4, name)));

let mat4uniform = name => Typed(protoMat4, RVar((Uniform, Mat4, name)));

let mat4var = name => Typed(protoMat4, RVar((Var, Mat4, name)));

let mat4output = name => Typed(protoMat4, RVar((Output, Mat4, name)));

let mat4arg = name => Typed(protoMat4, RVar((Arg, Mat4, name)));

/* End auto-generated by GLSLFunGen.py */
let intattr = name => Typed(`UInt, RVar((Attr, UInt, name)));

let intbuiltin = name => Typed(`Int, RVar((Builtin, Int, name)));

let int2float = l => Typed(protoScalar, BuiltinFun1("float", u(l)));

let sampler2Duniform = name =>
  Typed(`Sampler2D, RVar((Uniform, Sampler2D, name)));

let samplerCubeUniform = name =>
  Typed(`SamplerCube, RVar((Uniform, SamplerCube, name)));

/* Begin builtins and defaults */
/*
 let gl_Vertex = vec4builtin("gl_Vertex");
 */
let gl_Vertex = vec4attr("a_position");

let gl_VertexId = intbuiltin("gl_VertexID");

let gl_Position = vec4builtin("gl_Position");

let gl_FragCoord = vec4builtin("gl_FragCoord");

/*
 let gl_FragColor = vec4builtin("gl_FragColor");
 */
let outColor = vec4output("outColor");

let gl_FragColor = outColor;

let a_uv = vec2attr("a_uv");

let a_IndexId = intattr("a_IndexId");

let v_uv = vec2varying("v_uv");

let u_modelMatrix = mat4uniform("modelMatrix");

let u_modelViewMatrix = mat4uniform("modelViewMatrix");

let u_projectionMatrix = mat4uniform("projectionMatrix");

/* End of builtins and defaults */
let arStack = ref([]);

let ar = ref([]);

let vars = ref(SS.empty);

let add = x => {
  ar := [x, ...ar^];
  ();
};

let start = () => {
  ar := [];
  vars := SS.empty;
};

let finish = () => {
  let value = List.rev(ar^);
  value;
};

let push = () => {
  arStack := [ar^, ...arStack^];
  ar := [];
};

let pop = () => {
  let ret = finish();
  ar := List.hd(arStack^);
  arStack := List.tl(arStack^);
  ret;
};

let hasVar = var =>
  switch (SS.find(var, vars^)) {
  | _ => true
  | exception Not_found => false
  };

let addVar = var => {
  vars := SS.add(var, vars^);
  ();
};

let body = (x: unit => unit) => {
  start();
  x();
  finish();
};

let g1 = genericexpr1;

let g2 = genericexpr2;

let g2b = genericexpr2;

let glist = genericexprlist;

/*
 type assignType = (trT('a,'b), trT('b)) => unit
 constraint 'a = glslVariantTypeUBT
 constraint 'b = glslVariantTypeUBT('b);
 */
let (=@) = (l: trT('a), r: trT('a)) => add(assign(hasVar, addVar, u(l), u(r)));

let (++) = l => g1(l, Inc(u(l)));

let (--) = l => g1(l, Dec(u(l)));

let (+++) = l => g1(l, PreInc(u(l)));

let (---) = l => g1(l, PreDec(u(l)));

let (!) = l => g1(l, Not(u(l)));

let ( * ) = (l, r) =>
  Typed(PhantomAlgebra.( * )(getType(l), getType(r)), Mul(u(l), u(r)));

let ( *= ) = (l, r) => l =@ l * r;

let (/) = (l, r) =>
  Typed(PhantomAlgebra.(/)(getType(l), getType(r)), Div(u(l), u(r)));

let (/=) = (l, r) => l =@ l / r;

let (+) = (l, r) =>
  Typed(PhantomAlgebra.(+)(getType(l), getType(r)), Plus(u(l), u(r)));

let (+=) = (l, r) => l =@ l + r;

let (-) = (l, r) =>
  Typed(PhantomAlgebra.(-)(getType(l), getType(r)), Minus(u(l), u(r)));

let (-=) = (l, r) => l =@ l - r;

let (<) = (l, r) => g2b(l, r, LessThan(u(l), u(r)));

let (>) = (l, r) => g2b(l, r, GreaterThan(u(l), u(r)));

let (<=) = (l, r) => g2b(l, r, LessThanOrEqual(u(l), u(r)));

let (>=) = (l, r) => g2b(l, r, GreaterThanOrEqual(u(l), u(r)));

let (==) = (l, r) => g2b(l, r, Equal(u(l), u(r)));

let (!=) = (l, r) => g2b(l, r, NotEqual(u(l), u(r)));

let (&&) = (l, r) => g2b(l, r, And(u(l), u(r)));

let (||) = (l, r) => g2b(l, r, Or(u(l), u(r)));

let (^^) = (l, r) => g2b(l, r, Xor(u(l), u(r)));

let ( **. ) = (var, st) => {
  let (swizzletype, swizzle, validator) = st;
  let ret = Typed(swizzletype, RSwizzle(u(var), swizzle));
  validator(getType(var));
  ret;
};

let ternary: ('a, 'b, 'b) => 'b =
  (l, r1, r2) => Typed(getType(r1), Ternary(u(l), u(r1), u(r2)));

let max = (l, r) => g2(l, r, BuiltinFun2("max", u(l), u(r)));

let min = (l, r) => g2(l, r, BuiltinFun2("min", u(l), u(r)));

/* TODO: better type check */
let fmod = (l, r) => Typed(getType(l), BuiltinFun2("mod", u(l), u(r)));

let mix = (l, r1, r2) =>
  mixexpr3(l, r1, r2, BuiltinFun3("mix", u(l), u(r1), u(r2)));

/* TODO: type check */
let clamp = (l, r1, r2) =>
  Typed(getType(l), BuiltinFun3("clamp", u(l), u(r1), u(r2)));

let sqrt = l => g1(l, BuiltinFun1("sqrt", u(l)));

let sin = l => g1(l, BuiltinFun1("sin", u(l)));

let cos = l => g1(l, BuiltinFun1("cos", u(l)));

let abs = l => g1(l, BuiltinFun1("abs", u(l)));

let exp = l => g1(l, BuiltinFun1("exp", u(l)));

let dFdx = l => g1(l, BuiltinFun1("dFdx", u(l)));

let dFdy = l => g1(l, BuiltinFun1("dFdy", u(l)));

let length = l => genericexpr1float(l, BuiltinFun1("length", u(l)));

let normalize = l => g1(l, BuiltinFun1("normalize", u(l)));

let floor = l => g1(l, BuiltinFun1("floor", u(l)));

let fract = l => g1(l, BuiltinFun1("fract", u(l)));

let pow = (l, r) => g2(l, r, BuiltinFun2("pow", u(l), u(r)));

let cross = (l, r) => g2(l, r, BuiltinFun2("cross", u(l), u(r)));

let dot = (l, r) => genericexpr2float(l, r, BuiltinFun2("dot", u(l), u(r)));

let texture = (l: trT([ | `Sampler2D]), r: trT(PhantomAlgebra.vec2('a))) =>
  Typed(protoVec4, BuiltinFun2("texture", u(l), u(r)));

let textureCube = (l: trT([ | `SamplerCube]), r: trT(PhantomAlgebra.vec3('a))) =>
  Typed(protoVec4, BuiltinFun2("texture", u(l), u(r)));

/*switch (l, r) {
    /* | (Typed(`Sampler2D, _), Typed(`Vec2, _)) => Typed(`Vec4, Texture(l, r))*/
    | (Typed(`Sampler2D | `Vec2, _), Typed(`Sampler2D | `Vec2, _)) => (
      `Vec4,
      Texture(l, r)
    )
  };*/
let refract:
  (
    trT(t('dim, genType1T('rank1, 'rank2))),
    trT(t('dim, 'rank2)),
    trT(t('dim, Type_functions.z('a)))
  ) =>
  trT(t('dim, 'rank2)) =
  (l, r1, r2) => {
    let e = BuiltinFun3("refract", u(l), u(r1), u(r2));
    Typed(getType(l), e);
  };

let reflect:
  (trT(t('dim, genType1T('rank1, 'rank2))), trT(t('dim, 'rank2))) =>
  trT(t('dim, 'rank2)) =
  (l, r1) => {
    let e = BuiltinFun2("reflect", u(l), u(r1));
    Typed(getType(l), e);
  };

/*let vec2: cfun1 =*/
/*
 let vec2 = l => {
   let e = BuiltinFun("vec2", List.map(u, l));
   Typed(protoVec2, e);
 };

 let vec3: list(trT([< | `Float | `Vec2 | `Vec3])) => 'a =
   l => {
     let ret = Typed(`Vec3, BuiltinFun("vec3", List.map(u, l)));
     switch l {
     | [Typed(`Float, _)] => ret
     | [Typed(`Float, _), Typed(`Float, _), Typed(`Float, _)] => ret
     | [Typed(`Vec2, _), Typed(`Float, _)] => ret
     | [Typed(`Vec3, _)] => ret
     | _ => raise(GLSLTypeError("type mismatch"))
     };
   };
   */
let vec21f = (x: trT(PhantomAlgebra.scalar('a))) =>
  Typed(protoVec2, BuiltinFun("vec2", List.map(u, [x])));

let vec22f =
    (x: trT(PhantomAlgebra.scalar('a)), y: trT(PhantomAlgebra.scalar('a))) =>
  Typed(protoVec2, BuiltinFun("vec2", List.map(u, [x, y])));

let vec31f = (x: trT(PhantomAlgebra.scalar('a))) =>
  Typed(protoVec3, BuiltinFun("vec3", List.map(u, [x])));

let vec33f =
    (
      x: trT(PhantomAlgebra.scalar('a)),
      y: trT(PhantomAlgebra.scalar('a)),
      z: trT(PhantomAlgebra.scalar('a))
    ) =>
  Typed(protoVec3, BuiltinFun("vec3", List.map(u, [x, y, z])));

let (|+|) = (x, y) =>
  Typed(PhantomAlgebra.(|+|)(getType(x), getType(y)), Concat(u(x), u(y)));

let vec2 = (x: trT(PhantomAlgebra.vec2('a))) =>
  Typed(protoVec2, BuiltinFun("vec2", List.map(u, [x])));

let vec3 = (x: trT(PhantomAlgebra.vec3('a))) =>
  Typed(protoVec3, BuiltinFun("vec3", List.map(u, [x])));

let vec4 = (x: trT(PhantomAlgebra.vec4('a))) =>
  Typed(protoVec4, BuiltinFun("vec4", List.map(u, [x])));

let vec41f = (x: trT(PhantomAlgebra.scalar('a))) =>
  Typed(protoVec4, BuiltinFun("vec4", List.map(u, [x])));

let vec44f =
    (
      x: trT(PhantomAlgebra.scalar('a)),
      y: trT(PhantomAlgebra.scalar('a)),
      z: trT(PhantomAlgebra.scalar('a)),
      w: trT(PhantomAlgebra.scalar('a))
    ) =>
  Typed(protoVec4, BuiltinFun("vec4", List.map(u, [x, y, z, w])));

/*
 let vec4 = l => {
   let ret = Typed(`Vec4, BuiltinFun("vec4", List.map(u, l)));
   switch l {
   | [Typed(`Float, _)] => ret
   | [Typed(`Float, _), Typed(`Float, _), Typed(`Float, _), Typed(`Float, _)] => ret
   | [Typed(`Vec2, _), Typed(`Vec2, _)] => ret
   | [Typed(`Vec3, _), Typed(`Float, _)] => ret
   | [Typed(`Vec4, _)] => ret
   | _ => raise(GLSLTypeError("type mismatch"))
   };
 };
 */
/*
 let mat2 = l => BuiltinFun("mat2", l);

 let mat3 = l => BuiltinFun("mat3", l);

 let mat4 = l => BuiltinFun("mat4", l);
 */
let forstmt = (init, test, increment, body) => {
  let init = {
    push();
    init();
    pop();
  };
  /*
   let r1 = {
     push();
     r1();
     pop();
   };*/
  let increment = {
    push();
    increment();
    pop();
  };
  let body = {
    push();
    body();
    pop();
  };
  add(ForStatement(init, u(test), increment, body));
};

let ifstmt = (test, body) => {
  let body = {
    push();
    body();
    pop();
  };
  add(IfStatement(u(test), body));
};

let ifelsestmt = (l, body1, body2) => {
  let body1 = {
    push();
    body1();
    pop();
  };
  let body2 = {
    push();
    body2();
    pop();
  };
  add(IfElseStatement(u(l), body1, body2));
};

/* let f = x => Typed(`Float, ImmediateFloat(x)); */
/* let f: float => trT([ | `Float]) = x => Typed(`Float, ImmediateFloat(x)); */
let f = x => Typed(protoScalar, ImmediateFloat(x));

/* let f = x => Typed(`Float, ImmediateFloat(x)); */
let i = x => Typed(`Int, ImmediateInt(x));

let return = l => add(Return(u(l)));

let returnVoid = () => add(ReturnVoid);

let discard = () => add(Discard);

/* float var declaration and initialization */
/* TODO: figure out how to do this properly */
let floatif = a =>
  /*
   let var = floatvar(genSym());
   var =@ f(a);
   var;
   */
  f(a);

/* vec3 of 3 floats var declaration and initialization */
/* TODO: figure out how to do this properly */
let vec3i3f = (a, b, c) =>
  /*
   let var = vec3var(genSym());
   var =@ vec33f(f(a), f(b), f(c));
   var;
   */
  vec33f(f(a), f(b), f(c));

/* float of expr declaration and initialization */
/* TODO: figure out how to do this properly */
let floati1 = a => {
  let var = floatvar(genSym());
  var =@ a;
  var;
};

/* vec3 of expr declaration and initialization */
/* TODO: figure out how to do this properly */
let vec3i1 = a => {
  let var = vec3var(genSym());
  var =@ a;
  var;
};

/* TODO: Implement */
let const = x => x;

let nvec2init = (l, r) => vec22f(f(l), f(r));

let i1 = nvec2init(0.0, 0.0);

let i2 = f(0.0);

let d = mat4var("amat");

let a = sin(i1);

let b = sin(i2);

let c = vec2var("c");

let tmp = vec21f(b);

c =@ vec21f(b);

c =@ tmp;

finish();

();
