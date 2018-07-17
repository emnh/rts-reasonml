open! GLSL;

open GLSLUniforms;

let co = vec2arg("co");

let randBody =
  body(() =>
    return(
      fract(
        sin(dot(co **. xy', vec22f(f(12.9898), f(78.233)))) * f(43758.5453)
      )
    )
  );

let rand = x => fundecl1(floatfun("rand"), co, randBody, x);

let normalize = (x, y, z) => {
  let length = Math.sqrt(x *. x +. y *. y +. z *. z);
  (x /. length, y /. length, z /. length);
};

let c = vec3arg("c");

let hsv2rgbBody = 
  body(() =>
       {
         let k = vec4var("K");
         k =@ vec44f(f(1.0), f(2.0) / f(3.0), f(1.0) / f(3.0), f(3.0));
         let p = vec3var("p");
         p =@ abs(fract(c **. xxx' + k **. xyz') * f(6.0) - k **. www');
         return(c **. z' * mix(k **. xxx', clamp(p - k **. xxx', f(0.0), f(1.0)), c **. y'));
       }
  );

let hsv2rgb = x => fundecl1(vec3fun("rand"), c, hsv2rgbBody, x);
