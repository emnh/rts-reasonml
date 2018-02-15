let exampleVertexShader = {|#version 300 es

#define VARYING out
// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec4 a_position;
in vec2 a_uv;
VARYING vec2 v_uv;

layout(std140) uniform u_PerScene
{
	mediump mat4 modelViewMatrix;
	mediump mat4 projectionMatrix;
	mediump vec4 u_color1;
	mediump vec4 u_color2;
	mediump vec2 u_resolution;
	mediump float u_time;
};

// all shaders have a main function
void main() {

  // gl_Position is a special variable a vertex shader
  // is responsible for setting
	v_uv = a_uv;
  gl_Position = projectionMatrix * modelViewMatrix * vec4(a_position.xyz, 1.0);
	/* gl_Position = a_position; */
}
|};

let exampleFragmentShader = {|#version 300 es

// fragment shaders don't have a default precision so we need
// to pick one. mediump is a good default. It means "medium precision"
precision mediump float;

layout(std140) uniform u_PerScene
{
	mediump mat4 modelViewMatrix;
	mediump mat4 projectionMatrix;
	mediump vec4 u_color1;
	mediump vec4 u_color2;
	mediump vec2 u_resolution;
	mediump float u_time;
};

// we need to declare an output for the fragment shader
in vec2 v_uv;
out vec4 outColor;

void main() {
  // Just set the output to a constant redish-purple
  // outColor = vec4(1, 0, 0.5, 1);
	vec2 resolution = u_resolution;
	vec2 position = ( gl_FragCoord.xy / resolution.xy );
	position = v_uv;

	float time = u_time;
	float color = 0.0;
	color += sin(position.x * cos(time / 15.0) * 80.0) + cos(position.y * cos(time / 15.0) * 10.0);
	color += sin( position.y * sin( time / 10.0 ) * 40.0 ) + cos( position.x * sin( time / 25.0 ) * 40.0 );
	color += sin( position.x * sin( time / 5.0 ) * 10.0 ) + sin( position.y * sin( time / 35.0 ) * 80.0 );
	color *= sin( time / 10.0 ) * 0.5;

	outColor = vec4( vec3( color, color * 0.5, sin( color + time / 3.0 ) * 0.75 ), 1.0 );
  outColor += u_color1;
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

type bufferSubT;

type bufferT = Js.Nullable.t(bufferSubT);

type arrayBufferTypeT;

type uniformBufferTypeT = arrayBufferTypeT;

type elementArrayBufferT = arrayBufferTypeT;

type drawT;

type primitiveT;

type colorBufferBitT;

type vertexArrayT;

type drawGeometryT;

type uniformLocationT;

type uniformBlockIndexT;

type enableT;

[@bs.send]
external getUniformLocation : (glT, programT, string) => uniformLocationT =
  "getUniformLocation";

[@bs.send]
external getUniformBlockIndex : (glT, programT, string) => uniformBlockIndexT =
  "getUniformBlockIndex";

[@bs.send]
external uniformBlockBinding : (glT, programT, uniformBlockIndexT, int) => unit =
  "getUniformBlockIndex";

[@bs.send] [@bs.send]
external uniform1f : (glT, uniformLocationT, float) => unit = "uniform1f";

[@bs.send]
external uniform2f : (glT, uniformLocationT, float, float) => unit =
  "uniform2f";

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
external bufferData : (glT, arrayBufferTypeT, Float32Array.t, drawT) => unit =
  "bufferData";

[@bs.send]
external bufferDataInt16 : (glT, arrayBufferTypeT, Int16Array.t, drawT) => unit =
  "bufferData";

[@bs.send]
external bindBufferBase : (glT, arrayBufferTypeT, int, bufferT) => unit =
  "bindBufferBase";

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

[@bs.send]
external drawElements : (glT, drawGeometryT, int, primitiveT, int) => unit =
  "drawElements";

[@bs.get] external getVERTEX_SHADER : glT => shaderTypeT = "VERTEX_SHADER";

[@bs.get] external getFRAGMENT_SHADER : glT => shaderTypeT = "FRAGMENT_SHADER";

[@bs.get]
external getCOMPILE_STATUS : glT => compileStatusT = "COMPILE_STATUS";

[@bs.get] external getLINK_STATUS : glT => linkStatusT = "LINK_STATUS";

[@bs.get] external getARRAY_BUFFER : glT => arrayBufferTypeT = "ARRAY_BUFFER";

[@bs.get]
external getUNIFORM_BUFFER : glT => uniformBufferTypeT = "UNIFORM_BUFFER";

[@bs.get] external getSTATIC_DRAW : glT => drawT = "STATIC_DRAW";

[@bs.get] external getFLOAT : glT => primitiveT = "FLOAT";

[@bs.get] external getUNSIGNED_SHORT : glT => primitiveT = "UNSIGNED_SHORT";

[@bs.get]
external getELEMENT_ARRAY_BUFFER : glT => elementArrayBufferT =
  "ELEMENT_ARRAY_BUFFER";

[@bs.get]
external getCOLOR_BUFFER_BIT : glT => colorBufferBitT = "COLOR_BUFFER_BIT";

[@bs.get] external getTRIANGLES : glT => drawGeometryT = "TRIANGLES";

[@bs.get] external getDEPTH_TEST : glT => enableT = "DEPTH_TEST";

[@bs.send] external enable : (glT, enableT) => unit = "enable";

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

let testProgram =
    (
      gl,
      program,
      width,
      height,
      time,
      fgColor,
      bgColor,
      createGeometry,
      getObjectMatrix,
      getViewMatrices
    ) => {
  let box: Three.geometryBuffersT = createGeometry();
  let obj: Three.objectTransformT =
    getObjectMatrix(
      (0.0, 0.0, (-10.0)),
      (1.0, 1.0, 1.0),
      (
        Math.sin(time) *. 2.0 *. Math.pi,
        Math.sin(0.35 *. time) *. 2.0 *. Math.pi,
        Math.sin(0.73 *. time) *. 2.0 *. Math.pi
      )
    );
  let positions = box.position;
  let index = box.index;
  let positionBuffer = createBuffer(gl);
  bindBuffer(gl, getARRAY_BUFFER(gl), positionBuffer);
  bufferData(gl, getARRAY_BUFFER(gl), positions, getSTATIC_DRAW(gl));
  let vao = createVertexArray(gl);
  bindVertexArray(gl, vao);
  let positionAttributeLocation = getAttribLocation(gl, program, "a_position");
  enableVertexAttribArray(gl, positionAttributeLocation);
  let size = 3;
  let normalize = Js.Boolean.to_js_boolean(false);
  let stride = 0;
  let offset = 0;
  vertexAttribPointer(
    gl,
    positionAttributeLocation,
    size,
    getFLOAT(gl),
    normalize,
    stride,
    offset
  );
  let uvBuffer = createBuffer(gl);
  bindBuffer(gl, getARRAY_BUFFER(gl), uvBuffer);
  bufferData(gl, getARRAY_BUFFER(gl), box.uv, getSTATIC_DRAW(gl));
  let uvAttributeLocation = getAttribLocation(gl, program, "a_uv");
  enableVertexAttribArray(gl, uvAttributeLocation);
  let size = 2;
  let normalize = Js.Boolean.to_js_boolean(false);
  let stride = 0;
  let offset = 0;
  vertexAttribPointer(
    gl,
    uvAttributeLocation,
    size,
    getFLOAT(gl),
    normalize,
    stride,
    offset
  );
  let indexBuffer = createBuffer(gl);
  bindBuffer(gl, getELEMENT_ARRAY_BUFFER(gl), indexBuffer);
  bufferDataInt16(gl, getELEMENT_ARRAY_BUFFER(gl), index, getSTATIC_DRAW(gl));
  viewport(gl, 0, 0, width, height);
  enable(gl, getDEPTH_TEST(gl));
  clearColor(gl, 0, 0, 0, 0);
  clear(gl, getCOLOR_BUFFER_BIT(gl));
  useProgram(gl, program);
  bindVertexArray(gl, vao);
  let uniformBlockBindingIndex = 0;
  let uniformPerSceneLocation =
    getUniformBlockIndex(gl, program, "u_PerScene");
  uniformBlockBinding(
    gl,
    program,
    uniformPerSceneLocation,
    uniformBlockBindingIndex
  );
  let viewMatrices: Three.viewTransformT =
    getViewMatrices(obj.matrixWorld, width, height);
  let (r, g, b, _) = fgColor;
  let (r2, g2, b2, _) = bgColor;
  let c = 256.0;
  let uniformBlock =
    Float32Array.create(
      Array.concat([
        [|
          time,
          float_of_int(width),
          float_of_int(height),
          float_of_int(r) /. c,
          float_of_int(g) /. c,
          float_of_int(b) /. c,
          1.0,
          float_of_int(r2) /. c,
          float_of_int(g2) /. c,
          float_of_int(b2) /. c,
          1.0,
          0.0
        |],
        viewMatrices.modelViewMatrix,
        viewMatrices.projectionMatrix
      ])
    );
  let uniformPerSceneBuffer = createBuffer(gl);
  bindBuffer(gl, getUNIFORM_BUFFER(gl), uniformPerSceneBuffer);
  bufferData(gl, getUNIFORM_BUFFER(gl), uniformBlock, getSTATIC_DRAW(gl));
  bindBuffer(gl, getUNIFORM_BUFFER(gl), Js.Nullable.null);
  /* Render */
  bindBufferBase(
    gl,
    getUNIFORM_BUFFER(gl),
    uniformBlockBindingIndex,
    uniformPerSceneBuffer
  );
  /*
    let timeLocation = getUniformLocation(gl, program, "u_time");
    uniform1f(gl, timeLocation, time);
    let resolutionLocation = getUniformLocation(gl, program, "u_resolution");
    uniform2f(gl, resolutionLocation, float_of_int(width), float_of_int(height));
   */
  let offset = 0;
  let count = Int16Array.length(index);
  bindBuffer(gl, getARRAY_BUFFER(gl), positionBuffer);
  bindBuffer(gl, getELEMENT_ARRAY_BUFFER(gl), indexBuffer);
  /*
     drawArrays(
       gl,
       getTRIANGLES(gl),
       offset,
       Float32Array.length(positions) / size
     );
   */
  drawElements(gl, getTRIANGLES(gl), count, getUNSIGNED_SHORT(gl), offset);
};
