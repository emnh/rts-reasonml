/*
 * https://aphyr.com/posts/342-typing-the-technical-interview
 */
type scalar = float;

type z = pri | Z;

type s('a) = pri | S;

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

type rank1 = s(z);

type rank2 = s(s(z));

type t(_, _) =
  | Scalar(scalar): t('any_dim, 'any_rank)
  | Vec2(vec2): t(s(z), rank1)
  | Vec3(vec3): t(s(s(z)), rank1)
  | Vec4(vec4): t(s(s(s(z))), rank1)
  | Mat2(vec2, vec2): t(s(z), rank2)
  | Mat3(vec3, vec3, vec3): t(s(s(z)), rank2)
  | Mat4(vec4, vec4, vec4, vec4): t(s(s(s(z))), rank2);

/*
  * If yes, what you can do is adding a type parameter for the rank and model
  * the matrix as (rank 2, dim n) and then allow multiplication when dimension
  * matches, but addition only when both dimension and rank match(edited)
 if you don't want to broaden vectors to diagonal matrix, or other form of
 broadening
  * */
let (+) =
    (type dim, type rank, x: t(dim, rank), y: t(dim, rank))
    : t(dim, rank) => {
  let vec2plus = (a: vec2, b: vec2) => {x: a.x +. a.x, y: a.y +. b.y};
  let vec3plus = (a: vec3, b: vec3) => {
    x: a.x +. b.x,
    y: a.y +. b.y,
    z: a.z +. b.z
  };
  let vec4plus = (a: vec4, b: vec4) => {
    x: a.x +. b.x,
    y: a.y +. b.y,
    z: a.z +. b.z,
    w: a.w +. b.w
  };
  let vec2splus = ({x, y}: vec2, s) => {x: x +. s, y: y +. s};
  let vec3splus = ({x, y, z}: vec3, s) => {x: x +. s, y: y +. s, z: z +. s};
  let vec4splus = ({x, y, z, w}: vec4, s) => {
    x: x +. s,
    y: y +. s,
    z: z +. s,
    w: z +. w
  };
  switch (x, y) {
  | (Scalar(a), Scalar(b)) => Scalar(a +. b)
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

let ( * ) =
    (type dim, type rank, type r2, x: t(dim, rank), y: t(dim, r2))
    : t(dim, r2) => {
  /*
   let ( * ) =
       (type dim, type rank1, type rank2, type rank3, x: t(dim, rank1), y: t(dim, rank2))
       : t(dim, rank3) => {*/
  let vec2plus = (a: vec2, b: vec2) => {x: a.x *. a.x, y: a.y *. b.y};
  let vec3plus = (a: vec3, b: vec3) => {
    x: a.x *. b.x,
    y: a.y *. b.y,
    z: a.z *. b.z
  };
  let vec4plus = (a: vec4, b: vec4) => {
    x: a.x *. b.x,
    y: a.y *. b.y,
    z: a.z *. b.z,
    w: a.w *. b.w
  };
  let vec2splus = ({x, y}: vec2, s) => {x: x *. s, y: y *. s};
  let vec3splus = ({x, y, z}: vec3, s) => {x: x *. s, y: y *. s, z: z *. s};
  let vec4splus = ({x, y, z, w}: vec4, s) => {
    x: x *. s,
    y: y *. s,
    z: z *. s,
    w: z *. w
  };
  switch (x, y) {
  | (Scalar(a), Scalar(b)) => Scalar(a *. b)
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

let x = Scalar(1.) + Scalar(2.);

let o = Vec2({x: 1.0, y: 1.0}) + x;

let y = Vec2({x: 1.0, y: 1.0}) + Scalar(2.);

let z = Scalar(2.0) + Vec2({x: 1.0, y: 1.0});

let w = Vec3({x: 1.0, y: 1.0, z: 1.0}) + Scalar(2.);
