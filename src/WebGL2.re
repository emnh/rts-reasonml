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

type attributeLocationT = int;

type bufferSubT;

type bufferT = Js.Nullable.t(bufferSubT);

type arrayBufferTypeT;

type uniformBufferTypeT = arrayBufferTypeT;

type elementArrayBufferT = arrayBufferTypeT;

type drawT;

type primitiveT;

type colorBufferBitT = int;

type depthBufferBitT = colorBufferBitT;

type vertexArrayT;

type drawGeometryT;

type uniformLocationT;

type uniformBlockIndexT;

type enableT;

type textureT;

type clampT;

type texture2DT;

type textureParameterT;

type textureParameterValueT;

type rgbaT = int;

type textureIndexT;

type extensionT;

type framebufferT;

type framebufferTypeT;

type colorAttachmentT;

type queryT;

type queryTypeT;

type queryParameterT;

type parameterT;

type blend1T;

type blend2T;

[@bs.send]
external getUniformLocation : (glT, programT, string) => uniformLocationT =
  "getUniformLocation";

[@bs.send]
external getUniformBlockIndex : (glT, programT, string) => uniformBlockIndexT =
  "getUniformBlockIndex";

[@bs.send]
external uniformBlockBinding : (glT, programT, uniformBlockIndexT, int) => unit =
  "uniformBlockBinding";

[@bs.send]
external uniform1i : (glT, uniformLocationT, int) => unit = "uniform1i";

[@bs.send]
external uniform1iv : (glT, uniformLocationT, array(int)) => unit =
  "uniform1iv";

[@bs.send]
external uniform1f : (glT, uniformLocationT, float) => unit = "uniform1f";

[@bs.send]
external uniform1fv : (glT, uniformLocationT, array(float)) => unit =
  "uniform1fv";

[@bs.send]
external uniform2fv : (glT, uniformLocationT, array(float)) => unit =
  "uniform2fv";

[@bs.send]
external uniform3fv : (glT, uniformLocationT, array(float)) => unit =
  "uniform3fv";

[@bs.send]
external uniform4fv : (glT, uniformLocationT, array(float)) => unit =
  "uniform4fv";

[@bs.send]
external uniformMatrix4fv : (glT, uniformLocationT, bool, array(float)) => unit =
  "uniformMatrix4fv";

[@bs.send]
external uniform2f : (glT, uniformLocationT, float, float) => unit =
  "uniform2f";

[@bs.send]
external getContextPDB :
  (
    Document.element,
    string,
    [@bs.as {json|{ "preserveDrawingBuffer": true }|json}] _
  ) =>
  Js.Nullable.t(glT) =
  "getContext";

[@bs.send]
external getContext : (Document.element, string) => Js.Nullable.t(glT) =
  "getContext";

[@bs.send]
external createShader : (glT, shaderTypeT) => shaderT = "createShader";

[@bs.send] external createProgram : glT => programT = "createProgram";

[@bs.send] external createBuffer : glT => bufferT = "createBuffer";

[@bs.send] external createQuery : glT => queryT = "createQuery";

[@bs.send]
external beginQuery : (glT, queryTypeT, queryT) => unit = "beginQuery";

[@bs.send] external endQuery : (glT, queryTypeT) => unit = "endQuery";

[@bs.send]
external getQueryParameterBool : (glT, queryT, queryParameterT) => bool =
  "getQueryParameter";

[@bs.send]
external getQueryParameterInt : (glT, queryT, queryParameterT) => int =
  "getQueryParameter";

[@bs.send]
external getParameterBool : (glT, parameterT) => bool = "getParameter";

[@bs.send] external createTexture : glT => textureT = "createTexture";

[@bs.send]
external createFramebuffer : glT => framebufferT = "createFramebuffer";

[@bs.send]
external framebufferTexture2D :
  (glT, framebufferTypeT, colorAttachmentT, texture2DT, textureT, int) => unit =
  "framebufferTexture2D";

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

[@bs.send]
external checkFramebufferStatus : (glT, framebufferTypeT) => int =
  "checkFramebufferStatus";

[@bs.send] external deleteShader : (glT, shaderT) => unit = "deleteShader";

[@bs.send]
external attachShader : (glT, programT, shaderT) => unit = "attachShader";

[@bs.send] external getError : (glT) => unit = "getError";

[@bs.send] external linkProgram : (glT, programT) => unit = "linkProgram";

[@bs.send] external useProgram : (glT, programT) => unit = "useProgram";

[@bs.send] external deleteProgram : (glT, programT) => unit = "deleteProgram";

[@bs.send]
external getAttribLocation : (glT, programT, string) => attributeLocationT =
  "getAttribLocation";

[@bs.send]
external bindAttribLocation : (glT, programT, int, string) => unit =
  "bindAttribLocation";

[@bs.send]
external bindBuffer : (glT, arrayBufferTypeT, bufferT) => unit = "bindBuffer";

[@bs.send]
external bindTexture : (glT, texture2DT, textureT) => unit = "bindTexture";

[@bs.send]
external bindTexture2 : (glT, texture2DT, Js.Nullable.t(textureT)) => unit = "bindTexture";

[@bs.send]
external bindFramebuffer :
  (glT, framebufferTypeT, Js.Nullable.t(framebufferT)) => unit =
  "bindFramebuffer";

[@bs.send]
external bufferData : (glT, arrayBufferTypeT, Float32Array.t, drawT) => unit =
  "bufferData";

[@bs.send]
external bufferDataInt16 : (glT, arrayBufferTypeT, Int16Array.t, drawT) => unit =
  "bufferData";

[@bs.send]
external bufferDataInt32 : (glT, arrayBufferTypeT, Int32Array.t, drawT) => unit =
  "bufferData";

[@bs.send]
external bufferDataUint32 :
  (glT, arrayBufferTypeT, Uint32Array.t, drawT) => unit =
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
external disableVertexAttribArray : (glT, attributeLocationT) => unit =
  "disableVertexAttribArray";

[@bs.send]
external vertexAttribPointer :
  (glT, attributeLocationT, int, primitiveT, Js.boolean, int, int) => unit =
  "vertexAttribPointer";

[@bs.send] external viewport : (glT, int, int, int, int) => unit = "viewport";

[@bs.send]
external clearColor : (glT, float, float, float, float) => unit = "clearColor";

[@bs.send] external clear : (glT, colorBufferBitT) => unit = "clear";

[@bs.send]
external drawArrays : (glT, drawGeometryT, int, int) => unit = "drawArrays";

[@bs.send]
external drawElements : (glT, drawGeometryT, int, primitiveT, int) => unit =
  "drawElements";

[@bs.send]
external texParameteri :
  (glT, texture2DT, textureParameterT, textureParameterValueT) => unit =
  "texParameteri";

[@bs.send] external enable : (glT, enableT) => unit = "enable";

[@bs.send] external disable : (glT, enableT) => unit = "disable";

[@bs.send]
external texImage2D :
  (glT, texture2DT, int, rgbaT, rgbaT, primitiveT, Document.imageT) => unit =
  "texImage2D";

[@bs.send]
external texImage2Ddata :
  (
    glT,
    texture2DT,
    int,
    rgbaT,
    int,
    int,
    int,
    rgbaT,
    primitiveT,
    Uint8Array.t
  ) =>
  unit =
  "texImage2D";

[@bs.send]
external texImage2DdataFloat :
  (
    glT,
    texture2DT,
    int,
    rgbaT,
    int,
    int,
    int,
    rgbaT,
    primitiveT,
    Js.Nullable.t(Float32Array.t)
  ) =>
  unit =
  "texImage2D";

[@bs.send]
external activeTexture : (glT, textureIndexT) => unit = "activeTexture";

[@bs.send]
external generateMipmap : (glT, texture2DT) => unit = "generateMipmap";

[@bs.send]
external getExtension : (glT, string) => Js.Nullable.t(extensionT) =
  "getExtension";

[@bs.get] external getVERTEX_SHADER : glT => shaderTypeT = "VERTEX_SHADER";

[@bs.get] external getFRAGMENT_SHADER : glT => shaderTypeT = "FRAGMENT_SHADER";

[@bs.get]
external getCOMPILE_STATUS : glT => compileStatusT = "COMPILE_STATUS";

[@bs.get] external getLINK_STATUS : glT => linkStatusT = "LINK_STATUS";

[@bs.get] external getARRAY_BUFFER : glT => arrayBufferTypeT = "ARRAY_BUFFER";

[@bs.get]
external getUNIFORM_BUFFER : glT => uniformBufferTypeT = "UNIFORM_BUFFER";

[@bs.get] external getSTATIC_DRAW : glT => drawT = "STATIC_DRAW";

[@bs.get] external getDYNAMIC_DRAW : glT => drawT = "DYNAMIC_DRAW";

[@bs.get] external getFLOAT : glT => primitiveT = "FLOAT";

[@bs.get] external getUNSIGNED_BYTE : glT => primitiveT = "UNSIGNED_BYTE";

[@bs.get] external getUNSIGNED_SHORT : glT => primitiveT = "UNSIGNED_SHORT";

[@bs.get] external getINT : glT => primitiveT = "INT";

[@bs.get] external getUNSIGNED_INT : glT => primitiveT = "UNSIGNED_INT";

[@bs.get]
external getELEMENT_ARRAY_BUFFER : glT => elementArrayBufferT =
  "ELEMENT_ARRAY_BUFFER";

[@bs.get]
external getCOLOR_BUFFER_BIT : glT => colorBufferBitT = "COLOR_BUFFER_BIT";

[@bs.get]
external getDEPTH_BUFFER_BIT : glT => depthBufferBitT = "DEPTH_BUFFER_BIT";

[@bs.get] external getTRIANGLES : glT => drawGeometryT = "TRIANGLES";

[@bs.get] external getDEPTH_TEST : glT => enableT = "DEPTH_TEST";

[@bs.get] external getBLEND : glT => enableT = "BLEND";

[@bs.get] external getSRC_ALPHA : glT => blend1T = "SRC_ALPHA";

[@bs.get]
external getONE_MINUS_SRC_ALPHA : glT => blend1T = "ONE_MINUS_SRC_ALPHA";

[@bs.get] external getONE : glT => blend1T = "ONE";

[@bs.send] external blendFunc : (glT, blend1T, blend1T) => unit = "blendFunc";

[@bs.get]
external getCLAMP_TO_EDGE : glT => textureParameterValueT = "CLAMP_TO_EDGE";

[@bs.get] external getNEAREST : glT => textureParameterValueT = "NEAREST";

[@bs.get] external getLINEAR : glT => textureParameterValueT = "LINEAR";

[@bs.get]
external getLINEAR_MIPMAP_LINEAR : glT => textureParameterValueT =
  "LINEAR_MIPMAP_LINEAR";

[@bs.get] external getTEXTURE_2D : glT => texture2DT = "TEXTURE_2D";

[@bs.get] external getREPEAT : glT => textureParameterValueT = "REPEAT";

[@bs.get]
external getTEXTURE_WRAP_S : glT => textureParameterT = "TEXTURE_WRAP_S";

[@bs.get]
external getTEXTURE_WRAP_T : glT => textureParameterT = "TEXTURE_WRAP_T";

[@bs.get]
external getTEXTURE_MIN_FILTER : glT => textureParameterT =
  "TEXTURE_MIN_FILTER";

[@bs.get]
external getTEXTURE_MAG_FILTER : glT => textureParameterT =
  "TEXTURE_MAG_FILTER";

[@bs.get] external getRGBA : glT => rgbaT = "RGBA";

[@bs.get] external getRGBA32F : glT => rgbaT = "RGBA32F";

[@bs.get] external getRGBA16F_EXT : extensionT => rgbaT = "RGBA16F_EXT";

[@bs.get] external getTEXTURE0 : glT => textureIndexT = "TEXTURE0";

[@bs.get] external getTEXTURE1 : glT => textureIndexT = "TEXTURE1";

[@bs.get] external getTEXTURE2 : glT => textureIndexT = "TEXTURE2";

[@bs.get] external getTEXTURE3 : glT => textureIndexT = "TEXTURE3";

[@bs.get] external getTEXTURE4 : glT => textureIndexT = "TEXTURE4";

[@bs.get] external getTEXTURE5 : glT => textureIndexT = "TEXTURE5";

[@bs.get] external getTEXTURE6 : glT => textureIndexT = "TEXTURE6";

[@bs.get] external getTEXTURE7 : glT => textureIndexT = "TEXTURE7";

[@bs.get] external getTEXTURE8 : glT => textureIndexT = "TEXTURE8";

[@bs.get] external getTEXTURE9 : glT => textureIndexT = "TEXTURE9";

[@bs.get] external getTEXTURE10 : glT => textureIndexT = "TEXTURE10";

[@bs.get] external getTEXTURE11 : glT => textureIndexT = "TEXTURE11";

[@bs.get] external getTEXTURE12 : glT => textureIndexT = "TEXTURE12";

[@bs.get] external getTEXTURE13 : glT => textureIndexT = "TEXTURE13";

[@bs.get] external getTEXTURE14 : glT => textureIndexT = "TEXTURE14";

[@bs.get] external getTEXTURE15 : glT => textureIndexT = "TEXTURE15";

[@bs.get] external getFRAMEBUFFER : glT => framebufferTypeT = "FRAMEBUFFER";

[@bs.get]
external getCOLOR_ATTACHMENT0 : glT => colorAttachmentT = "COLOR_ATTACHMENT0";

[@bs.get]
external getTIME_ELAPSED_EXT : extensionT => queryTypeT = "TIME_ELAPSED_EXT";

[@bs.get]
external getQUERY_RESULT_AVAILABLE : glT => queryParameterT =
  "QUERY_RESULT_AVAILABLE";

[@bs.get] external getQUERY_RESULT : glT => queryParameterT = "QUERY_RESULT";

[@bs.get]
external getGPU_DISJOINT_EXT : extensionT => parameterT = "GPU_DISJOINT_EXT";

[@bs.set] external setMY_VERSION : (glT, int) => unit = "MY_VERSION";

[@bs.get] external getMY_VERSION : glT => int = "MY_VERSION";
