open! Core;

/*
let slice3 :
(
      t('dim1, Indexing.superindexing('rank1, 'rank2, 'rank3, 'dim1, 'dim3, 'len, _)),
      index('dim1, 'len, 'rank2, 'group)
    ) =>
    t('dim3, 'rank3)
  = (x, y) => slice(x, y);
  */

/*
let slice2 = (t: Interface.Indexing.t(_, _), n: Interface.Indexing.index(_)) => slice(t, n);
*/

let (|?) = (x, name) => {
  Format.printf("%s=%a@.", name, pp, x);
  x;
};

let v = vec2(0., 1.) |? "v";

let m = mat2(vec2(1., 0.), scalar(2.) * v) |? "m";

let m2 = m * m |? "m2";

let ez = v * m |? "ee";

let ew = vec3(0., 0., 1.) |? "ew";

let s = scalar(7.) |? "s";

let ew2 = v + vec2(1., 1.) |? "ew2";

let v' = s + v |? "v'";

let v'' = s * v |? "v''";

let v''' = v' * (scalar(1.) + v'') |? "v'''";

let xyrot = theta =>
  mat3(
    vec3(cos(theta), sin(theta), 0.),
    vec3(-. sin(theta), cos(theta), 0.),
    vec3(0., 0., 1.)
  );

let pi = 4. *. atan(1.);

let r = xyrot(pi /. 6.) |? "r";

let e1 = vec3(1., 0., 0.) |? "e1";

let e2 = xyrot(pi /. 2.) * e1 |? "e2";

 let f = slice(v, x) |? "f";
 /*
 let ryy = r.%[yy] |? "ryy"
 let rx = r.%[x] |? "rx"
let e3 = cross(e1, e2) |? "e3";
 */

let f2 = e1 ++ e2 |? "f2";

let m_one = cross(vec2(0., 1.), vec2(1., 0.)) |? "m_one";

let v4 = scalar(0.) |+| vec2(1., 2.) |+| scalar(3.) |? "v4";

let v4' = vec2(1., 2.) |+| vec2(3., 4.) |? "v4'";

let sel = w & z & y & x;

/*
 let sw = v4 .%[sel] |? "sw"
 */
let eye = mat2(vec2(1., 0.), vec2(0., 1.)) |? "eye";

/*
 let msw = eye.%[y&x] |? "msw"

 let diag = eye.%[xx&yy] |? "diag"
 let stretch = vec4' (vec2 3. 2. |+| scalar 1.)
 */
/*
 let error = cross (vec4 0. 0. 0. 1.) (vec4 0. 1. 0. 0.)
 let error' = cross (scalar 1.) (scalar 2.)
 */
module X: {let t: scalar(_);} = {
  let fn = (v, w) => cross(v, w) + scalar(1.);
  let t = fn(vec2(0., 1.), vec2(1., 0.));
};
