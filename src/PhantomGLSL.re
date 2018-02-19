/*
 * https://gist.github.com/Octachron/4e833a22844fd90cd6d15b0af927dab0
 * Thanks to Octachron for all his help! :)
 *
 * Resources:
 *
 * The + in type parameter:
 * https://blog.janestreet.com/a-and-a/
 *
 * The & in variant types:
 * https://www.math.nagoya-u.ac.jp/~garrigue/papers/variant-reuse.pdf
 * */
type scalar = float;

type one('a) = [ | `one('a)];

type z('a) = [ | `zero('a)];

type two('a) = [ | `two('a)];

type three('a) = [ | `three('a)];

type four('a) = [ | `four('a)];

let map2 = (f, x, y) =>
  Array.init(min(Array.length(x), Array.length(y)), n => f(x[n], y[n]));

type any('a, 'b, 'c) =
  [< | `zero('b) &('a) | `one('b) &('a) | `two('b) &('a)] as 'c;

/** (x,y,z,_ ) product computes the rank of x * y and
    put the result inside z */
type product('a, 'b, 'c, 'parameters) =
  [<
    | `zero
        ('b)
        &(
          [<
            | `zero('c) &(z('p1))
            | `one('c) &(one('p1))
            | `two('c) &(two('p1))
          ]
        )
    | `one
        ('b)
        &(
          [<
            | `zero('c) &(one('p1))
            | `one('c) &(one('p1))
            | `two('c) &(one('p1))
          ]
        )
    | `two
        ('b)
        &(
          [<
            | `zero('c) &(two('p1))
            | `one('c) &(one('p1))
            | `two('c) &(two('p1))
          ]
        )
  ] as 'a
constraint 'parameters = ('p1, 'p2, 'p3);

/** (x,y,z,_ ) sum computes the rank of x + y and
    put the result inside z */
type sum('a, 'b, 'c, 'parameters) =
  [<
    | `zero
        ('b)
        &(
          [<
            | `zero('c) &(z('p1))
            | `one('c) &(one('p1))
            | `two('c) &(two('p1))
          ]
        )
    | `one('b) &([< | `zero('c) &(one('p1)) | `one('c) &(one('p1))])
    | `two('b) &([< | `zero('c) &(two('p1)) | `two('c) &(two('p1))])
  ] as 'a
constraint 'parameters = ('p1, 'p2, 'p3);

exception Unexpected_matrix_dimension;

exception Unexpected_ranks(int, int);

module Phantom: {
  type t(+'dim, +'rank);
  type scalar(+'x) = t('a, z('b)) constraint 'x = ('a, 'b);
  type vec2(+'x) = t(two('a), one('b)) constraint 'x = ('a, 'b);
  type vec3(+'x) = t(three('a), one('b)) constraint 'x = ('a, 'b);
  type vec4(+'x) = t(four('a), one('b)) constraint 'x = ('a, 'b);
  type mat2(+'x) = t(two('a), two('b)) constraint 'x = ('a, 'b);
  type mat3(+'x) = t(three('a), two('b)) constraint 'x = ('a, 'b);
  type mat4(+'x) = t(four('a), two('b)) constraint 'x = ('a, 'b);
  let scalar: float => scalar(_);
  let vec2: (float, float) => vec2(_);
  let vec3: (float, float, float) => vec3(_);
  let vec4: (float, float, float, float) => vec4(_);
  let mat2: (vec2(_), vec2(_)) => mat2(_);
  let mat3: (vec3(_), vec3(_), vec3(_)) => mat3(_);
  let mat4: (vec4(_), vec4(_), vec4(_), vec4(_)) => mat4(_);
  let (+):
    (t('a, sum('rank1, 'rank2, 'rank3, _)), t('a, 'rank2)) => t('a, 'rank3);
  let ( * ):
    (t('dim, product('rank1, 'rank2, 'rank3, _)), t('dim, 'rank2)) =>
    t('dim, 'rank3);
  let floor: scalar(_) => int;
} = {
  type t(+'dim, +'rank) = {
    rank: int,
    data: array(float)
  };
  type scalar(+'x) = t('a, z('b)) constraint 'x = ('a, 'b);
  type vec2(+'x) = t(two('a), one('b)) constraint 'x = ('a, 'b);
  type vec3(+'x) = t(three('a), one('b)) constraint 'x = ('a, 'b);
  type vec4(+'x) = t(four('a), one('b)) constraint 'x = ('a, 'b);
  type mat2(+'x) = t(two('a), two('b)) constraint 'x = ('a, 'b);
  type mat3(+'x) = t(three('a), two('b)) constraint 'x = ('a, 'b);
  type mat4(+'x) = t(four('a), two('b)) constraint 'x = ('a, 'b);
  let scalar = x => {rank: 0, data: [|x|]};
  let vec2 = (x, y) => {rank: 1, data: [|x, y|]};
  let vec3 = (x, y, z) => {rank: 1, data: [|x, y, z|]};
  let vec4 = (x, y, z, t) => {rank: 1, data: [|x, y, z, t|]};
  let mat2 = ({data: a, _}, {data: b, _}) => {
    rank: 2,
    data: [|a[0], a[1], b[0], b[1]|]
  };
  let mat3 = ({data: a, _}, {data: b, _}, {data: c, _}) => {
    rank: 2,
    data: [|a[0], a[1], a[2], b[0], b[1], b[2], c[0], c[1], c[2]|]
  };
  let mat4 = ({data: a, _}, {data: b, _}, {data: c, _}, {data: d, _}) => {
    rank: 2,
    data: [|
      a[0],
      a[1],
      a[2],
      a[3],
      b[0],
      b[1],
      b[2],
      b[3],
      c[0],
      c[1],
      c[2],
      c[3],
      d[0],
      d[1],
      d[2],
      d[3]
    |]
  };
  let map = (f, x) => {...x, data: Array.map(f, x.data)};
  let smap = (f, x, y) => map(f(x.data[0]), y);
  let dim = a =>
    switch (Array.length(a.data)) {
    | 4 => 2
    | 9 => 3
    | 16 => 4
    | _ => raise(Unexpected_matrix_dimension)
    };
  let ( * ) = (a, b) =>
    switch (a.rank, b.rank) {
    | (0, _) => smap(( *. ), a, b)
    | (_, 0) => smap(( *. ), b, a)
    | (1, 1) => {rank: 1, data: map2(( *. ), a.data, b.data)}
    | (1, 2)
    | (2, 1) =>
      let dim = Array.length(a.data);
      let (a, b, s1, s2) =
        if (a.rank == 1) {
          (a.data, b.data, dim, 1);
        } else {
          (b.data, a.data, 1, dim);
        };
      let sum = i => {
        let s = ref(0.)
        and ij = ref(i * s1);
        for (j in 0 to dim) {
          s := a[j] *. b[ij^] +. s^;
          ij := s2 + ij^;
        };
        s^;
      };
      {rank: 1, data: Array.init(dim, sum)};
    | (2, 2) =>
      let dim = dim(a);
      let a = a.data
      and b = b.data;
      let sum = (i, j) => {
        let s = ref(0.);
        for (k in 0 to dim) {
          s := a[i * dim + k] *. b[k * dim + j] +. s^;
        };
        s^;
      };
      let data = Array.init(Array.length(a), n => sum(n / dim, n mod dim));
      {rank: 2, data};
    | (x, y) => raise([@implicit_arity] Unexpected_ranks(x, y))
    };
  let (+) = (a, b) =>
    if (a.rank == 0) {
      smap((+.), a, b);
    } else if (b.rank == 0) {
      smap((+.), b, a);
    } else {
      {...a, data: map2((+.), a.data, b.data)};
    };
  let floor = a => int_of_float(a.data[0]);
};

open! Phantom;

let v = vec2(0., 1.);

let m = mat2(v, v);

let x = m * m;

let z = v * m;

let w = vec3(0., 0., 1.);

let w' = w + scalar(0.);

let s = scalar(0.);

let w = v + vec2(1., 1.);

let v' = s + v;

let v'' = s * v;

let v''' = v' * v'';

/*
 let error = vec3(0., 0., 1.) + vec2(1., 0.);

 let error2 = vec3(0., 0., 1.) * vec2(1., 0.);
 */
let tests = {
  let s = scalar(0.);
  let v2 = vec2(0.0, 1.0);
  let v3 = vec3(0.0, 1.0, 2.0);
  let v4 = vec4(0.0, 1.0, 2.0, 3.0);
  let m2 = mat2(v2, v2);
  let m3 = mat3(v3, v3, v3);
  let m4 = mat4(v4, v4, v4, v4);
  let s' = s + s;
  let v2' = v2 + s;
  let v2'' = s + v2;
  let e2 = v2' == v2'';
  let v3' = v3 + s;
  let v3'' = s + v3;
  let e3 = v3 == v3'';
  let v4' = s + v4;
  let v4'' = v4 + s;
  let e4 = v4' == v4'';
  let m2' = (m2 + m2 + s) * v2;
  let m3' = (m3 + m3 + s) * v3;
  let m4' = (m4 + m4 + s) * v4;
  ();
};
