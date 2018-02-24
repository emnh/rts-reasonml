/*
 // Description : Array and textureless GLSL 2D simplex noise function.
 //      Author : Ian McEwan, Ashima Arts.
 //  Maintainer : stegu
 //     Lastmod : 20110822 (ijm)
 //     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
 //               Distributed under the MIT License. See LICENSE file.
 //               https://github.com/ashima/webgl-noise
 //               https://github.com/stegu/webgl-noise
 */
open! GLSL;

/*
 let (mod289_2, mod289_3) = {
   let x2 = vec2arg("x");
   let x3 = vec3arg("x");
   let mod289_body = x =>
     body(
       () => {
         return(x - floor(x * (f(1.0) / f(289.0))) * f(289.0));
       }
     );
   (
     fundecl(vec2fun("mod289"), [x2], mod289_body(x2)),
     fundecl(vec3fun("mod289"), [x3], mod289_body(x3))
   );
 };
 */
let x = vec3arg("x");

let x2 = vec2arg("x");

let x3 = vec3arg("x");

let mod289_2_body =
  body(() => return(x2 - floor(x2 * (f(1.0) / f(289.0))) * f(289.0)));

let mod289_3_body =
  body(() => return(x3 - floor(x3 * (f(1.0) / f(289.0))) * f(289.0)));

let mod289_2 = x => fundecl1(vec2fun("mod289"), x2, mod289_2_body, x);

let mod289_3 = x => fundecl1(vec3fun("mod289"), x3, mod289_3_body, x);

let permute_body = body(() => return(mod289_3((x * f(34.0) + f(1.0)) * x)));

let permute = arg => fundecl1(vec3fun("permute"), x, permute_body, arg);

let v = vec2arg("v");

let snoise_body =
  body(() => {
    let cC = vec4var("C");
    cC
    =@ vec44f(
         f(0.211324865405187), /* (3.0-sqrt(3.0))/6.0 */
         f(0.366025403784439), /* 0.5*(sqrt(3.0)-1.0) */
         f(-0.577350269189626), /* -1.0 + 2.0 * C.x */
         f(0.024390243902439)
       ); /* 1.0 / 41.0 */
    /* First corner */
    let i = vec2var("i");
    i =@ floor(v + dot(v, cC **. yy'));
    let x0 = vec2var("x0");
    x0 =@ v - i + dot(i, cC **. xx');
    /* Other corners */
    let i1 = vec2var("i1");
    i1
    =@ ternary(
         x0 **. x' > x0 **. y',
         vec22f(f(1.0), f(0.0)),
         vec22f(f(0.0), f(1.0))
       );
    let x12 = vec4var("x12");
    x12 =@ x0 **. xyxy' + cC **. xxzz';
    x12 **. xy' -= i1;
    /* Permutations */
    i =@ mod289_2(i); /* Avoid truncation effects in permutation */
    let p = vec3var("p");
    p
    =@ permute(
         permute(i **. y' + vec33f(f(0.0), i1 **. y', f(1.0)))
         + i
         **. x'
         + vec33f(f(0.0), i1 **. x', f(1.0))
       );
    let m = vec3var("m");
    m
    =@ max(
         f(0.5)
         - vec33f(
             dot(x0, x0),
             dot(x12 **. xy', x12 **. xy'),
             dot(x12 **. zw', x12 **. zw')
           ),
         vec31f(f(0.0))
       );
    m =@ m * m;
    m =@ m * m;
    /* Gradients: 41 points uniformly over a line, mapped onto a diamond. */
    /* The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287) */
    let x = vec3var("x");
    let h = vec3var("h");
    let ox = vec3var("ox");
    let a0 = vec3var("a0");
    x =@ f(2.0) * fract(p * (cC **. www')) - f(1.0);
    h =@ abs(x) - f(0.5);
    ox =@ floor(x + f(0.5));
    a0 =@ x - ox;
    /* Normalise gradients implicitly by scaling m */
    /* Approximation of: m *= inversesqrt( a0*a0 + h*h ); */
    m *= (f(1.79284291400159) - f(0.85373472095314) * (a0 * a0 + h * h));
    /* Compute final noise value at P */
    let g = vec3var("g");
    g =@ vec31f(f(0.0));
    g **. x' =@ a0 **. x' * (x0 **. x') + h **. x' * (x0 **. y');
    g **. yz' =@ a0 **. yz' * (x12 **. xz') + h **. yz' * (x12 **. yw');
    return(f(130.0) * dot(m, g));
  });

let snoise = arg => fundecl1(floatfun("snoise"), v, snoise_body, arg);
