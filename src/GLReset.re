type glResetModuleT;

[@bs.module "gl-reset/state"] external glResetModule : glResetModuleT = "";

let createReset: (WebGL2.glT, unit) => unit =
  gl => {
    let _ = glResetModule;
    let _ = gl;
    Js.log(gl);
    /* [%bs.raw {| GlReset(arguments[0]) |}];*/
    [%bs.raw {| State(gl) |}];
  };
