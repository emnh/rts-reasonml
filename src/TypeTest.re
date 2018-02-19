/*
 * Resources:
 *
 * Tagless final style:
 * http://okmij.org/ftp/tagless-final/index.html
 *
 * Strange Haskell type adventure:
 * https://aphyr.com/posts/342-typing-the-technical-interview
 */
exception TypeError(string);

type scalar = float;

type z = pri | Z;

type s('a) = pri | S;

/*
 type pbool = | True | False;
 */
/*
 module type Or {
   type b1;
   type b2;
   type b;

   let get : (b1, b2) => b;
 };

 type ptrue;
 type pfalse;

 module type PBool {
   type a = | True | False;
 };

 module Or = (B1 : PBool, B2 : PBool) => {
   type a = switch (B1.a, B2.a) {
     | (True, True) => ptrue;
   };
 };
 */
type vec2 = {
  x: float,
  y: float
};

type vec3 = {
  x: float,
  y: float,
  z: float
};

type vec4 = {
  x: float,
  y: float,
  z: float,
  w: float
};

type p0 = z;

type p1 = s(z);

type p2 = s(s(z));

type s0 = [ | `Scalar];

type s1 = [ | `Scalar | `VecMat];

type s0b('a) = [< s0] as 'a;

type s1b('a) = [< s1] as 'a;

type t(_, _, _, 'a) =
  | Scalar(scalar): t('any_dim, 'any_left_rank, 'any_right_rank, s0b('a))
  | Vec2(vec2): t(s(z), p1, p0, s1b('a))
  | Vec3(vec3): t(s(s(z)), p1, p0, s1b('a))
  | Vec4(vec4): t(s(s(s(z))), p1, p0, s1b('a))
  | Mat2(vec2, vec2): t(s(z), p1, p1, s1b('a))
  | Mat3(vec3, vec3, vec3): t(s(s(z)), p1, p1, s1b('a))
  | Mat4(vec4, vec4, vec4, vec4): t(s(s(s(z))), p1, p1, s1b('a));

/*
  * If yes, what you can do is adding a type parameter for the rank and model
  * the matrix as (rank 2, dim n) and then allow multiplication when dimension
  * matches, but addition only when both dimension and rank match(edited)
 if you don't want to broaden vectors to diagonal matrix, or other form of
 broadening
  * */
let broadcastOp =
    (
      op,
      type dim,
      type left_rank,
      type right_rank,
      type isScalar,
      x: t(dim, left_rank, right_rank, isScalar),
      y: t(dim, left_rank, right_rank, isScalar)
    )
    : t(dim, left_rank, right_rank, isScalar) => {
  let vec2plus = (a: vec2, b: vec2) => {x: op(a.x, b.x), y: op(a.y, b.y)};
  let vec3plus = (a: vec3, b: vec3) => {
    x: op(a.x, b.x),
    y: op(a.y, b.y),
    z: op(a.z, b.z)
  };
  let vec4plus = (a: vec4, b: vec4) => {
    x: op(a.x, b.x),
    y: op(a.y, b.y),
    z: op(a.z, b.z),
    w: op(a.w, b.w)
  };
  let vec2splus = ({x, y}: vec2, s) => {x: op(x, s), y: op(y, s)};
  let vec3splus = ({x, y, z}: vec3, s) => {
    x: op(x, s),
    y: op(y, s),
    z: op(z, s)
  };
  let vec4splus = ({x, y, z, w}: vec4, s) => {
    x: op(x, s),
    y: op(y, s),
    z: op(z, s),
    w: op(w, w)
  };
  switch (x, y) {
  | (Scalar(a), Scalar(b)) => Scalar(op(a, b))
  | (Scalar(s), Vec2(a)) => Vec2(vec2splus(a, s))
  | (Scalar(s), Vec3(a)) => Vec3(vec3splus(a, s))
  | (Scalar(s), Vec4(a)) => Vec4(vec4splus(a, s))
  | (Scalar(s), Mat2(a, b)) => Mat2(vec2splus(a, s), vec2splus(b, s))
  | (Scalar(s), Mat3(a, b, c)) =>
    Mat3(vec3splus(a, s), vec3splus(b, s), vec3splus(c, s))
  | (Scalar(s), Mat4(a, b, c, d)) =>
    Mat4(vec4splus(a, s), vec4splus(b, s), vec4splus(c, s), vec4splus(d, s))
  | (Mat2(a, b), Scalar(s)) => Mat2(vec2splus(a, s), vec2splus(b, s))
  | (Mat3(a, b, c), Scalar(s)) =>
    Mat3(vec3splus(a, s), vec3splus(b, s), vec3splus(c, s))
  | (Mat4(a, b, c, d), Scalar(s)) =>
    Mat4(vec4splus(a, s), vec4splus(b, s), vec4splus(c, s), vec4splus(d, s))
  | (Vec2(a), Scalar(s)) => Vec2(vec2splus(a, s))
  | (Vec3(a), Scalar(s)) => Vec3(vec3splus(a, s))
  | (Vec4(a), Scalar(s)) => Vec4(vec4splus(a, s))
  | (Vec2(a), Vec2(b)) => Vec2(vec2plus(a, b))
  | (Vec3(a), Vec3(b)) => Vec3(vec3plus(a, b))
  | (Vec4(a), Vec4(b)) => Vec4(vec4plus(a, b))
  | (Mat2(a, b), Mat2(c, d)) => Mat2(vec2plus(a, c), vec2plus(b, d))
  | (Mat3(a, b, c), Mat3(d, e, f)) =>
    Mat3(vec3plus(a, d), vec3plus(b, e), vec3plus(c, f))
  | (Mat4(a, b, c, d), Mat4(e, f, g, h)) =>
    Mat4(vec4plus(a, e), vec4plus(b, f), vec4plus(c, g), vec4plus(d, h))
  };
};

let (+) = broadcastOp((+.));

let (-) = broadcastOp((-.));

let ( * ) = broadcastOp(( *. ));

let (/) = broadcastOp((/.));

/*
 type cs('a, 'b) = 'a constraint 'a = s('b);
 */
let ( ** ) =
    (
      type dim,
      type left_rank,
      type inner_rank,
      type right_rank,
      x: t(dim, left_rank, inner_rank, s1),
      y: t(dim, inner_rank, right_rank, s1)
    )
    : t(dim, left_rank, right_rank, s1) =>
  switch (x, y) {
  | (Mat2(_, _), Vec2(e)) => Vec2(e)
  | (Mat2(_, _), Mat2(a, b)) => Mat2(a, b)
  | (Mat3(_, _, _), Vec3(e)) => Vec3(e)
  | (Mat3(_, _, _), Mat3(a, b, c)) => Mat3(a, b, c)
  | (Mat4(_, _, _, _), Vec4(e)) => Vec4(e)
  | (Mat4(_, _, _, _), Mat4(a, b, c, d)) => Mat4(a, b, c, d)
  /* (1, 0) * (0, r) => (1, r) but there is no type with (0, r) except the forbidden scalar */
  | (Vec2(_), _) => raise(TypeError("cannot happen"))
  | (Vec3(_), _) => raise(TypeError("cannot happen"))
  | (Vec4(_), _) => raise(TypeError("cannot happen"))
  /*
   | _ => raise(TypeError("type mismatch"))
   | (Scalar(a), Scalar(b)) => Scalar(a *. b)
   | (Scalar(s), Vec2(a)) => Vec2(a)
   | (Scalar(s), Vec3(a)) => Vec3(vec3splus(a, s))
   | (Scalar(s), Vec4(a)) => Vec4(vec4splus(a, s))
   | (Scalar(s), Mat2(a, b)) => Mat2(vec2splus(a, s), vec2splus(b, s))
   | (Scalar(s), Mat3(a, b, c)) =>
     Mat3(vec3splus(a, s), vec3splus(b, s), vec3splus(c, s))
   | (Scalar(s), Mat4(a, b, c, d)) =>
     Mat4(vec4splus(a, s), vec4splus(b, s), vec4splus(c, s), vec4splus(d, s))
   | (Mat2(a, b), Scalar(s)) => Mat2(vec2splus(a, s), vec2splus(b, s))
   | (Mat3(a, b, c), Scalar(s)) =>
     Mat3(vec3splus(a, s), vec3splus(b, s), vec3splus(c, s))
   | (Mat4(a, b, c, d), Scalar(s)) =>
     Mat4(vec4splus(a, s), vec4splus(b, s), vec4splus(c, s), vec4splus(d, s))
   | (Vec2(a), Scalar(s)) => Vec2(vec2splus(a, s))
   | (Vec3(a), Scalar(s)) => Vec3(vec3splus(a, s))
   | (Vec4(a), Scalar(s)) => Vec4(vec4splus(a, s))
   | (Vec2(a), Vec2(b)) => Vec2(vec2plus(a, b))
   | (Vec3(a), Vec3(b)) => Vec3(vec3plus(a, b))
   | (Vec4(a), Vec4(b)) => Vec4(vec4plus(a, b))
   | (Mat2(a, b), Mat2(c, d)) => Mat2(vec2plus(a, c), vec2plus(b, d))
   | (Mat3(a, b, c), Mat3(d, e, f)) =>
     Mat3(vec3plus(a, d), vec3plus(b, e), vec3plus(c, f))
   | (Mat4(a, b, c, d), Mat4(e, f, g, h)) =>
     Mat4(vec4plus(a, e), vec4plus(b, f), vec4plus(c, g), vec4plus(d, h))
     */
  };

let x = Scalar(1.) + Scalar(2.);

let o = Vec2({x: 1.0, y: 1.0}) + x;

let y = Vec2({x: 1.0, y: 1.0}) + Scalar(2.);

let yy = Vec2({x: 1.0, y: 1.0}) * Scalar(2.);

let z = Scalar(2.0) + Vec2({x: 1.0, y: 1.0});

let w = Vec3({x: 1.0, y: 1.0, z: 1.0}) + Scalar(2.);

let typeError1 = Scalar(1.) ** Scalar(2.);

let typeError2 = Vec2({x: 1.0, y: 1.0}) ** Vec2({x: 1.0, y: 1.0});
