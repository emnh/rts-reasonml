let exampleVertexShader = {|#version 300 es

// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec4 a_position;

// all shaders have a main function
void main() {

  // gl_Position is a special variable a vertex shader
  // is responsible for setting
  gl_Position = a_position;
}
|};

let exampleFragmentShader = {|#version 300 es

// fragment shaders don't have a default precision so we need
// to pick one. mediump is a good default. It means "medium precision"
precision mediump float;

// we need to declare an output for the fragment shader
out vec4 outColor;

void main() {
  // Just set the output to a constant redish-purple
  outColor = vec4(1, 0, 0.5, 1);
}
|};

type webGL2RenderingContextT = Document.element;

type glT = webGL2RenderingContextT;

type shaderT;

type shaderTypeT;

type shaderParameterT;

type programT;

type programParameterT;

type compileStatusT;

type linkStatusT;

type attributeLocationT;

type bufferT;

type arrayBufferTypeT;

type float32ArrayT;

type drawT;

type primitiveT;

type colorBufferBitT;

type vertexArrayT;

type drawGeometryT;

[@bs.new]
external createFloat32Array : array(float) => float32ArrayT = "Float32Array";

[@bs.send]
external getContext : (Document.element, string) => Js.Nullable.t(glT) =
  "getContext";

[@bs.send]
external createShader : (glT, shaderTypeT) => shaderT = "createShader";

[@bs.send] external createProgram : glT => programT = "createProgram";

[@bs.send] external createBuffer : glT => bufferT = "createBuffer";

[@bs.send]
external createVertexArray : glT => vertexArrayT = "createVertexArray";

[@bs.send]
external shaderSource : (glT, shaderT, string) => unit = "shaderSource";

[@bs.send] external compileShader : (glT, shaderT) => unit = "compileShader";

[@bs.send]
external getShaderParameter : (glT, shaderT, compileStatusT) => Js.boolean =
  "getShaderParameter";

[@bs.send]
external getProgramParameter : (glT, programT, linkStatusT) => Js.boolean =
  "getProgramParameter";

[@bs.send]
external getShaderInfoLog : (glT, shaderT) => string = "getShaderInfoLog";

[@bs.send]
external getProgramInfoLog : (glT, programT) => string = "getProgramInfoLog";

[@bs.send] external deleteShader : (glT, shaderT) => unit = "deleteShader";

[@bs.send]
external attachShader : (glT, programT, shaderT) => unit = "attachShader";

[@bs.send] external linkProgram : (glT, programT) => unit = "linkProgram";

[@bs.send] external useProgram : (glT, programT) => unit = "useProgram";

[@bs.send] external deleteProgram : (glT, programT) => unit = "deleteProgram";

[@bs.send]
external getAttribLocation : (glT, programT, string) => attributeLocationT =
  "getAttribLocation";

[@bs.send]
external bindBuffer : (glT, arrayBufferTypeT, bufferT) => unit = "bindBuffer";

[@bs.send]
external bufferData : (glT, arrayBufferTypeT, float32ArrayT, drawT) => unit =
  "bufferData";

[@bs.send]
external bindVertexArray : (glT, vertexArrayT) => unit = "bindVertexArray";

[@bs.send]
external enableVertexAttribArray : (glT, attributeLocationT) => unit =
  "enableVertexAttribArray";

[@bs.send]
external vertexAttribPointer :
  (glT, attributeLocationT, int, primitiveT, Js.boolean, int, int) => unit =
  "vertexAttribPointer";

[@bs.send] external viewport : (glT, int, int, int, int) => unit = "viewport";

[@bs.send]
external clearColor : (glT, int, int, int, int) => unit = "clearColor";

[@bs.send] external clear : (glT, colorBufferBitT) => unit = "clear";

[@bs.send]
external drawArrays : (glT, drawGeometryT, int, int) => unit = "drawArrays";

[@bs.get] external getVERTEX_SHADER : glT => shaderTypeT = "VERTEX_SHADER";

[@bs.get] external getFRAGMENT_SHADER : glT => shaderTypeT = "FRAGMENT_SHADER";

[@bs.get]
external getCOMPILE_STATUS : glT => compileStatusT = "COMPILE_STATUS";

[@bs.get] external getLINK_STATUS : glT => linkStatusT = "LINK_STATUS";

[@bs.get] external getARRAY_BUFFER : glT => arrayBufferTypeT = "ARRAY_BUFFER";

[@bs.get] external getSTATIC_DRAW : glT => drawT = "STATIC_DRAW";

[@bs.get] external getFLOAT : glT => primitiveT = "FLOAT";

[@bs.get]
external getCOLOR_BUFFER_BIT : glT => colorBufferBitT = "COLOR_BUFFER_BIT";

[@bs.get] external getTRIANGLES : glT => drawGeometryT = "TRIANGLES";

let createShader = (gl, stype, source) => {
  let shader = createShader(gl, stype);
  shaderSource(gl, shader, source);
  compileShader(gl, shader);
  let success = getShaderParameter(gl, shader, getCOMPILE_STATUS(gl));
  if (Js.to_bool(success)) {
    Some(shader);
  } else {
    Js.log(getShaderInfoLog(gl, shader));
    deleteShader(gl, shader);
    None;
  };
};

let createProgram = (gl, vertexShader, fragmentShader) => {
  let program = createProgram(gl);
  attachShader(gl, program, vertexShader);
  attachShader(gl, program, fragmentShader);
  linkProgram(gl, program);
  let success = getProgramParameter(gl, program, getLINK_STATUS(gl));
  if (Js.to_bool(success)) {
    Some(program);
  } else {
    Js.log(getProgramInfoLog(gl, program));
    deleteProgram(gl, program);
    None;
  };
};

let testProgram = (gl, program, width, height) => {
  let positionAttributeLocation = getAttribLocation(gl, program, "a_position");
  let positionBuffer = createBuffer(gl);
  bindBuffer(gl, getARRAY_BUFFER(gl), positionBuffer);
  let positions = [|0.0, 0.0, 0.0, 0.5, 0.7, 0.0|];
  bufferData(
    gl,
    getARRAY_BUFFER(gl),
    createFloat32Array(positions),
    getSTATIC_DRAW(gl)
  );
  let vao = createVertexArray(gl);
  bindVertexArray(gl, vao);
  enableVertexAttribArray(gl, positionAttributeLocation);
  let size = 2;
  let ttype = getFLOAT(gl);
  let normalize = Js.Boolean.to_js_boolean(false);
  let stride = 0;
  let offset = 0;
  vertexAttribPointer(
    gl,
    positionAttributeLocation,
    size,
    ttype,
    normalize,
    stride,
    offset
  );
  viewport(gl, 0, 0, width, height);
  clearColor(gl, 0, 0, 0, 0);
  clear(gl, getCOLOR_BUFFER_BIT(gl));
  useProgram(gl, program);
  bindVertexArray(gl, vao);
  let primitiveType = getTRIANGLES(gl);
  let offset = 0;
  let count = 3;
  drawArrays(gl, primitiveType, offset, count);
};
