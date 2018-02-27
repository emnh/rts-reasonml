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
