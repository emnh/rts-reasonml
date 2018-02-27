/*
 * WebGL Water
 * http://madebyevan.com/webgl-water/
 *
 * Copyright 2011 Evan Wallace
 * Ported to ReasonML by Eivind Magnus Hvidevold
 * Released under the MIT license
 */
open! GLSL;

open GLSLUniforms;

let u_resolution = vec2uniform("u_resolution");

let eye = vec3uniform("u_eye");

let water = sampler2Duniform("t_water");

let heightMap = sampler2Duniform("t_heightmap");

let heightBaseMultiplier = f(0.1);

let heightMultiplier = heightBaseMultiplier / ShaderCopy.maxHeight;

let u_time = floatuniform("u_time");

let waveHeight = floatuniform("u_waterOffset");

let memGRT =
  Memoize.partialMemoize3((gl, height, offset) =>
    getNewRandomTexture(gl, i =>
      switch (i mod 4) {
      | 0 => Math.random() *. height +. offset
      | 1 => 0.0
      | _ => 0.0
      }
    )
  );

module Renderer = {
  /* TODO: bool */
  let uvMul =
    vec22f(f(2.0) / f(Terrain.getWidth()), f(2.0) / f(Terrain.getHeight()));
  let uvMul3 =
    vec33f(
      f(2.0) / f(Terrain.getWidth()),
      f(1.0),
      f(2.0) / f(Terrain.getHeight())
    );
  let ocoord = vec2varying("ocoord");
  let isCaustics = floatuniform("u_isCaustics");
  let waterHeight = floatuniform("u_waterHeight");
  let light = vec3uniform("u_light");
  let sphereCenter = vec3uniform("u_sphereCenter");
  let sphereRadius = floatuniform("u_sphereRadius");
  let tiles = sampler2Duniform("t_tiles");
  let terrain = sampler2Duniform("t_terrain");
  let causticTex = sampler2Duniform("t_causticTex");
  let cIOR_AIR = const(floatif(1.0));
  let cIOR_WATER = const(floatif(1.333));
  let abovewaterColor = const(vec3i3f(0.25, 1.0, 1.25) * f(1.5));
  let underwaterColor = const(vec3i3f(0.4, 0.9, 1.0));
  let poolHeight = floatuniform("poolHeight"); /* const(floatif(1.5)); */
  let origin = vec3arg("origin");
  let ray = vec3arg("ray");
  let cubeMin = vec3arg("cubeMin");
  let cubeMax = vec3arg("cubeMax");
  let position = vec3varying("v_position");
  let waterDepth = floatvarying("v_waterDepth");
  /* Set cubeLimit to high number for no walls, 1.0 for standard walls */
  let cubeLimit = f(25.0);
  let wallPosition = cubeLimit - f(0.001);
  let globalCubeMin =
    vec33f(cubeLimit * f(-1.0), f(0.0) - poolHeight, cubeLimit * f(-1.0));
  let globalCubeMax = vec33f(cubeLimit * f(1.0), f(2.0), cubeLimit * f(1.0));
  let sky = samplerCubeUniform("u_sky");
  /* TODO: bool */
  let u_isAboveWater = floatuniform("u_isAboveWater");
  /*
   let gl_Vertex = gl_Vertex **. xzyw';
   */
  let gl_Vertex = gl_Vertex;
  let crRayOrigin = vec3arg("rayOrigin");
  let crRayDelta = vec3arg("rayDelta");
  let crMaxT = f(2.0);
  let castRayBody =
    body(() => {
      /* Modified from https://www.shadertoy.com/view/4scGW7 */
      let mint = f(0.0);
      let maxt = crMaxT;
      let dt = maxt / f(10.0);
      let lh = floatvar("lh");
      let ly = floatvar("ly");
      lh =@ f(0.0);
      ly =@ f(0.0);
      let t = floatvar("t");
      t =@ mint;
      forstmt(
        () => t =@ mint,
        t < maxt,
        () => t += dt,
        () => {
          let p = vec3var("p");
          p =@ crRayOrigin + crRayDelta * t;
          let h = floatvar("h");
          h
          =@ texture(heightMap, fract(p **. xz' * f(0.5) + f(0.5)))
          **. x'
          * heightMultiplier
          - poolHeight;
          h =@ min(crRayOrigin **. y', h);
          ifstmt(p **. y' <= h, () =>
            return(t - dt + dt * (lh - ly) / (p **. y' - ly - h + lh))
          );
          lh =@ h;
          ly =@ p **. y';
        }
      );
      return(maxt * f(2.0));
    });
  let castRay = (x, y) =>
    fundecl2(
      floatfun("castRay"),
      (crRayOrigin, crRayDelta),
      castRayBody,
      x,
      y
    );
  let intersectCubeBody =
    body(() => {
      /* ifstmt(isCaustics < f(0.0), () => cubeMin **. y' =@ waterDepth); */
      let tMin = vec3var("tMin");
      tMin =@ (cubeMin - origin) / ray;
      let tMax = vec3var("tMax");
      tMax =@ (cubeMax - origin) / ray;
      let t1 = vec3var("t1");
      t1 =@ min(tMin, tMax);
      let t2 = vec3var("t2");
      t2 =@ max(tMin, tMax);
      let tNear = floatvar("tNear");
      let tFar = floatvar("tFar");
      tNear =@ max(max(t1 **. x', t1 **. y'), t1 **. z');
      tFar =@ min(min(t2 **. x', t2 **. y'), t2 **. z');
      /*
       let tMid = floatvar("tMid");
       tMid =@ castRay(origin, ray);
       ifstmt(tMid <= crMaxT, () => tFar =@ min(tFar, tMid));
       */
      return(vec2(tNear |+| tFar));
    });
  let intersectCube = (x, y, z, w) =>
    fundecl4(
      vec2fun("intersectCube"),
      (origin, ray, cubeMin, cubeMax),
      intersectCubeBody,
      x,
      y,
      z,
      w
    );
  let argSphereCenter = vec3arg("sphereCenter");
  let argSphereRadius = floatarg("sphereRadius");
  let intersectSphereBody =
    body(() => {
      let toSphere = vec3i1(origin - argSphereCenter);
      let a = floati1(dot(ray, ray));
      let b = floati1(f(2.0) * dot(toSphere, ray));
      let c =
        floati1(dot(toSphere, toSphere) - argSphereRadius * argSphereRadius);
      let discriminant = floatvar("discriminant");
      discriminant =@ b * b - f(4.0) * a * c;
      ifstmt(
        discriminant > f(0.0),
        () => {
          let t = floatvar("t");
          t =@ (f(0.0) - b - sqrt(discriminant)) / (f(2.0) * a);
          ifstmt(t > f(0.0), () => return(t));
        }
      );
      return(f(1.0e6));
    });
  let intersectSphere = (x, y, z, w) =>
    fundecl4(
      floatfun("intersectSphere"),
      (origin, ray, argSphereCenter, argSphereRadius),
      intersectSphereBody,
      x,
      y,
      z,
      w
    );
  let point = vec3arg("point");
  let getSphereColorBody =
    body(() => {
      let color = vec3var("color");
      color =@ vec31f(f(0.5));
      /* ambient occlusion with walls */
      color
      *= (
        f(1.0)
        - f(0.9)
        / pow(
            (f(1.0) + sphereRadius - abs(point **. x')) / sphereRadius,
            f(3.0)
          )
      );
      color
      *= (
        f(1.0)
        - f(0.9)
        / pow(
            (f(1.0) + sphereRadius - abs(point **. z')) / sphereRadius,
            f(3.0)
          )
      );
      color
      *= (
        f(1.0)
        - f(0.9)
        / pow((point **. y' + f(1.0) + sphereRadius) / sphereRadius, f(3.0))
      );
      /* caustics */
      let sphereNormal = vec3var("sphereNormal");
      sphereNormal =@ (point - sphereCenter) / sphereRadius;
      let refractedLight = vec3var("refractedLight");
      refractedLight
      =@ refract(
           f(0.0) - light,
           vec33f(f(0.0), f(1.0), f(0.0)),
           cIOR_AIR / cIOR_WATER
         );
      let diffuse = floatvar("diffuse");
      diffuse
      =@ max(f(0.0), dot(f(0.0) - refractedLight, sphereNormal))
      * f(0.5);
      let info = vec4var("info");
      info =@ texture(water, point **. xz' * f(0.5) * uvMul + f(0.5));
      ifstmt(
        point **. y' < info **. r',
        () => {
          let caustic = vec4var("caustic");
          caustic
          =@ texture(
               causticTex,
               f(0.75)
               * (
                 point
                 **. xz'
                 * uvMul
                 - point
                 **. y'
                 * (refractedLight **. xz')
                 / (refractedLight **. y')
               )
               * f(0.5)
               + f(0.5)
             );
          diffuse *= (caustic **. r' * f(4.0));
        }
      );
      color += diffuse;
      return(color);
    });
  let getSphereColor = pt =>
    fundecl1(vec3fun("getSphereColor"), point, getSphereColorBody, pt);
  let displacementPoint = vec3arg("displacementPoint");
  let displacementBody =
    body(() => {
      let point = vec3var("point");
      point =@ displacementPoint;
      point
      **. y'
      =@ displacementPoint
      **. y'
      * ShaderAshima.snoise(displacementPoint **. xz' * f(10.0) + u_time);
      return(point);
    });
  let displacement = pt =>
    fundecl1(vec3fun("displacement"), displacementPoint, displacementBody, pt);
  let getWallColorBody =
    body(() => {
      let scale = floatvar("scale");
      scale =@ f(0.5);
      let wallColor = vec3var("wallColor");
      wallColor =@ vec31f(f(0.0));
      let normal = vec3var("normal");
      normal =@ vec31f(f(0.0));
      let uv = vec2var("uv");
      uv =@ point **. xz' * f(0.5) * uvMul + f(0.5);
      /* Triangle wave to tile terrain. */
      /* uv =@ abs(fmod(uv, f(2.0)) - f(1.0)); */
      /* Sine wave to tile terrain. Hacky, but looks better for now. */
      uv =@ sin(uv);
      let color = texture(terrain, uv);
      let gray = dot(color **. rgb', vec33f(f(0.299), f(0.587), f(0.114)));
      wallColor =@ vec31f(gray);
      normal =@ texture(heightMap, uv) **. yzw';
      scale /= length(point * uvMul3); /* pool ambient occlusion */
      /* caustics */
      let refractedLight = vec3var("refractedLight");
      refractedLight
      =@ f(0.0)
      - refract(
          f(0.0) - light,
          vec33f(f(0.0), f(1.0), f(0.0)),
          cIOR_AIR / cIOR_WATER
        );
      let diffuse = floatvar("diffuse");
      diffuse =@ max(f(0.0), dot(refractedLight, normal));
      let info = vec4var("info");
      info =@ texture(water, point **. xz' * f(0.5) * uvMul + f(0.5));
      ifelsestmt(
        point **. y' < info **. r',
        () => {
          let caustic = vec4var("caustic");
          let causticUV = vec2var("causticUV");
          causticUV
          =@ f(0.75)
          * (
            point
            **. xz'
            * uvMul
            - point
            **. y'
            * (refractedLight **. xz')
            / (refractedLight **. y')
          )
          * f(0.5)
          + f(0.5);
          /* Just add some fake repeated caustics at edge of map */
          /* TODO: do it in a better way */
          let cl = f(0.25);
          causticUV
          **. x'
          =@ ternary(
               causticUV **. x' < cl,
               f(2.0) * cl - causticUV **. x',
               causticUV **. x'
             );
          causticUV
          **. x'
          =@ ternary(
               causticUV **. x' > f(1.0) - cl,
               f(2.0) * (f(1.0) - cl) - causticUV **. x',
               causticUV **. x'
             );
          causticUV
          **. y'
          =@ ternary(
               causticUV **. y' < cl,
               f(2.0) * cl - causticUV **. y',
               causticUV **. y'
             );
          causticUV
          **. y'
          =@ ternary(
               causticUV **. y' > f(1.0) - cl,
               f(2.0) * (f(1.0) - cl) - causticUV **. y',
               causticUV **. y'
             );
          caustic =@ texture(causticTex, causticUV);
          scale += diffuse * (caustic **. r') * f(2.0) * (caustic **. g');
        },
        () => ()
      );
      return(wallColor * scale * f(1.5));
    });
  let getWallColor = pt =>
    fundecl1(vec3fun("getWallColor"), point, getWallColorBody, pt);
  let origin = vec3arg("origin");
  let ray = vec3arg("ray");
  let waterColor = vec3arg("waterColor");
  let getSurfaceRayColorBody =
    body(() => {
      let color = vec3var("color");
      color =@ vec31f(f(0.0));
      ifelsestmt(
        ray **. y' < f(0.0),
        () => {
          let t = vec2var("t1");
          t =@ intersectCube(origin, ray, globalCubeMin, globalCubeMax);
          let hit = vec3var("hit1");
          hit =@ origin + ray * (t **. y');
          color =@ getWallColor(hit);
        },
        () => {
          let t = vec2var("t2");
          t =@ intersectCube(origin, ray, globalCubeMin, globalCubeMax);
          let hit = vec3var("hit2");
          hit =@ origin + ray * (t **. y');
          ifelsestmt(
            hit **. y' < f(2.0) / f(12.0),
            () => color =@ getWallColor(hit),
            () => {
              let uvray = ray * vec33f(uvMul **. x', f(1.0), uvMul **. y');
              color =@ textureCube(sky, uvray) **. rgb';
              color
              += vec31f(pow(max(f(0.0), dot(light, ray)), f(5000.0)))
              * vec33f(f(10.0), f(8.0), f(6.0));
            }
          );
        }
      );
      ifstmt(ray **. y' < f(0.0), () => color *= waterColor);
      return(color);
    });
  let getSurfaceRayColor = (x, y, z) =>
    fundecl3(
      vec3fun("getSurfaceRayColor"),
      (origin, ray, waterColor),
      getSurfaceRayColorBody,
      x,
      y,
      z
    );
  let transitionDelta = f(0.0);
  let isWater = floatvarying("isWater");
  let waterVertexShader =
    body(() => {
      v_uv =@ a_uv;
      let info = vec4var("info");
      let uvc = vec2var("uvc");
      uvc =@ gl_Vertex **. xy' * f(0.5) * uvMul + f(0.5);
      info =@ texture(water, uvc);
      let height = floatvar("height");
      height =@ texture(heightMap, uvc) **. x';
      let waterHeight2 = floatvar("waterHeight2");
      waterHeight2 =@ info **. r';
      position =@ gl_Vertex **. xzy';
      isWater =@ waterHeight2 - (f(1.0) - transitionDelta) * height;
      waterDepth =@ position **. y' + height * heightMultiplier * f(5.0);
      /* waterDepth =@ position **. y' - poolHeight; */
      ifelsestmt(
        isWater < f(0.0),
        () => position **. y' += height * heightMultiplier,
        () => position **. y' += waterHeight2 * heightMultiplier
      );
      ocoord =@ position **. xz' * f(0.5) * uvMul + f(0.5);
      gl_Position
      =@ u_projectionMatrix
      * u_modelViewMatrix
      * vec4(position **. xyz' |+| f(1.0));
    });
  let waterFragmentShader =
    body(() => {
      let position2 = vec3var("position2");
      position2 =@ position;
      let coord = vec2var("ocoord");
      let info = vec4var("info");
      coord =@ ocoord;
      info =@ texture(water, ocoord);
      /* make water look more "peaked" */
      /*
       /* TODO: int var */
       let i = floatvar("i");
       forstmt(
         () => i =@ f(0.0),
         i < f(5.0),
         () => i += f(1.0),
         () => {
           coord += info **. ba' * f(0.005);
           info =@ texture(water, coord);
         }
       );
       */
      let normal = vec3var("normal");
      let incomingRay = vec3var("incomingRay");
      normal
      =@ vec33f(
           info **. b',
           sqrt(f(1.0) - dot(info **. ba', info **. ba')),
           info **. a'
         );
      incomingRay =@ normalize(position2 - eye);
      ifelsestmt(
        u_isAboveWater < f(0.0),
        () => {
          /* underwater */
          normal =@ f(0.0) - normal;
          let reflectedRay = vec3var("reflectedRay1");
          let refractedRay = vec3var("refractedRay1");
          let fresnel = floatvar("fresnel1");
          reflectedRay =@ reflect(incomingRay, normal);
          refractedRay =@ refract(incomingRay, normal, cIOR_WATER / cIOR_AIR);
          fresnel
          =@ mix(
               f(0.5),
               f(1.0),
               pow(f(1.0) - dot(normal, f(0.0) - incomingRay), f(3.0))
             );
          let reflectedColor = vec3var("reflectedColor1");
          let refractedColor = vec3var("refractedColor1");
          reflectedColor
          =@ getSurfaceRayColor(position2, reflectedRay, underwaterColor);
          refractedColor
          =@ getSurfaceRayColor(position2, refractedRay, vec31f(f(1.0)))
          * vec33f(f(0.8), f(1.0), f(1.1));
          gl_FragColor
          =@ vec4(
               mix(
                 reflectedColor,
                 refractedColor,
                 (f(1.0) - fresnel) * length(refractedRay)
               )
               |+| f(1.0)
             );
        },
        () => {
          /* above water */
          let reflectedRay = vec3var("reflectedRay2");
          let refractedRay = vec3var("refractedRay2");
          let fresnel = floatvar("fresnel2");
          reflectedRay =@ reflect(incomingRay, normal);
          refractedRay =@ refract(incomingRay, normal, cIOR_AIR / cIOR_WATER);
          fresnel
          =@ mix(
               f(0.25),
               f(1.0),
               pow(f(1.0) - dot(normal, f(0.0) - incomingRay), f(3.0))
             );
          let reflectedColor = vec3var("reflectedColor2");
          let refractedColor = vec3var("refractedColor2");
          reflectedColor
          =@ getSurfaceRayColor(position2, reflectedRay, abovewaterColor);
          refractedColor
          =@ getSurfaceRayColor(position2, refractedRay, abovewaterColor);
          gl_FragColor
          =@ vec4(mix(refractedColor, reflectedColor, fresnel) |+| f(1.0));
          let color = texture(terrain, ocoord) **. rgb';
          gl_FragColor
          =@ ternary(isWater < f(0.0), vec4(color |+| f(1.0)), gl_FragColor);
        }
      );
    });
  /* XXX: for debug */
  /*
   gl_FragColor =@ texture(tiles, v_uv);
   */
  /*
   gl_FragColor **. rg' =@ v_uv;
   */
  let sphereVertexShader =
    body(() => {
      position =@ sphereCenter + gl_Vertex **. xyz' * sphereRadius;
      gl_Position
      =@ u_projectionMatrix
      * u_modelViewMatrix
      * vec4(position |+| f(1.0));
    });
  let sphereFragmentShader =
    body(() => {
      gl_FragColor =@ vec4(getSphereColor(position) |+| f(1.0));
      let info = vec4var("info");
      info =@ texture(water, position **. xz' * f(0.5) + f(0.5));
      ifstmt(position **. y' < info **. r', () =>
        gl_FragColor **. rgb' *= (underwaterColor * f(1.2))
      );
    });
  let cubeVertexShader =
    body(() => {
      position =@ gl_Vertex **. xyz';
      position
      **. y'
      =@ ((f(1.0) - position **. y') * (f(7.0) / f(12.0)) - f(1.0))
      * poolHeight;
      gl_Position
      =@ u_projectionMatrix
      * u_modelViewMatrix
      * vec4(position |+| f(1.0));
    });
  let cubeFragmentShader =
    body(() => {
      gl_FragColor =@ vec4(getWallColor(position) |+| f(1.0));
      let info = vec4var("info");
      info =@ texture(water, position **. xz' * f(0.5) + f(0.5));
      ifstmt(position **. y' < info **. r', () =>
        gl_FragColor **. rgb' *= (underwaterColor * f(1.2))
      );
    });
  let oldPos = vec3varying("oldPos");
  let newPos = vec3varying("newPos");
  let ray = vec3varying("ray");
  let refractedLight = vec3arg("refractedLight");
  let projectBody =
    body(() => {
      /* project the ray onto the plane */
      let tcube = vec2var("tcube");
      tcube =@ intersectCube(origin, ray, globalCubeMin, globalCubeMax);
      origin += ray * (tcube **. y');
      let tplane = floatvar("tplane");
      tplane =@ (f(0.0) - origin **. y' - f(1.0)) / (refractedLight **. y');
      return(origin + refractedLight * tplane);
    });
  let project = (x, y, z) =>
    fundecl3(
      vec3fun("project"),
      (origin, ray, refractedLight),
      projectBody,
      x,
      y,
      z
    );
  let causticsVertexShader =
    body(() => {
      let info = vec4var("info");
      let gl_Vertex = vec3(gl_Vertex **. xy' * uvMul |+| gl_Vertex **. z');
      info =@ texture(water, gl_Vertex **. xy' * f(0.5) + f(0.5));
      info **. ba' *= f(0.5);
      let normal = vec3var("normal");
      normal
      =@ vec33f(
           info **. b',
           sqrt(f(1.0) - dot(info **. ba', info **. ba')),
           info **. a'
         );
      /* project the vertices along the refracted vertex ray */
      let refractedLight = vec3var("refractedLight");
      refractedLight
      =@ refract(
           f(0.0) - light,
           vec33f(f(0.0), f(1.0), f(0.0)),
           cIOR_AIR / cIOR_WATER
         );
      ray =@ refract(f(0.0) - light, normal, cIOR_AIR / cIOR_WATER);
      oldPos =@ project(gl_Vertex **. xzy', refractedLight, refractedLight);
      newPos
      =@ project(
           gl_Vertex **. xzy' + vec33f(f(0.0), info **. r', f(0.0)),
           ray,
           refractedLight
         );
      gl_Position
      =@ vec4(
           f(0.75)
           * (
             newPos **. xz' + refractedLight **. xz' / (refractedLight **. y')
           )
           |+| f(0.0)
           |+| f(1.0)
         );
    });
  let causticsFragmentShader =
    body(() => {
      /* if the triangle gets smaller, it gets brighter, and vice versa */
      let oldArea = floatvar("oldArea");
      let newArea = floatvar("newArea");
      oldArea =@ length(dFdx(oldPos)) * length(dFdy(oldPos));
      newArea =@ length(dFdx(newPos)) * length(dFdy(newPos));
      gl_FragColor
      =@ vec44f(oldArea / newArea * f(0.2), f(1.0), f(0.0), f(0.0));
      let refractedLight = vec3var("refractedLight");
      refractedLight
      =@ refract(
           f(0.0) - light,
           vec33f(f(0.0), f(1.0), f(0.0)),
           cIOR_AIR / cIOR_WATER
         );
      /* compute a blob shadow and make sure we only draw a shadow if the player is blocking the light */
      let dir = vec3var("dir");
      dir =@ (sphereCenter - newPos) / sphereRadius;
      let area = vec3var("area");
      area =@ cross(dir, refractedLight);
      let shadow = floatvar("shadow");
      shadow =@ dot(area, area);
      let dist = floatvar("dist");
      dist =@ dot(dir, f(0.0) - refractedLight);
      shadow =@ f(1.0) + (shadow - f(1.0)) / (f(0.05) + dist * f(0.025));
      shadow =@ clamp(f(1.0) / (f(1.0) + exp(f(0.0) - shadow)), f(0.0), f(1.0));
      shadow =@ mix(f(1.0), shadow, clamp(dist * f(2.0), f(0.0), f(1.0)));
      gl_FragColor **. g' =@ shadow;
      /* shadow for the rim of the pool */
      let t = vec2var("t");
      t
      =@ intersectCube(
           newPos,
           f(0.0) - refractedLight,
           globalCubeMin,
           globalCubeMax
         );
      gl_FragColor
      **. r'
      *= (
        f(1.0)
        / (
          f(1.0)
          + exp(
              f(-200.0)
              / (f(1.0) + f(10.0) * (t **. y' - t **. x'))
              * (
                newPos
                **. y'
                - refractedLight
                **. y'
                * (t **. y')
                - f(2.0)
                / f(12.0)
              )
            )
        )
      );
    });
  let r = registerUniform;
  let tilesTexture = ref(None);
  let registeredTiles =
    registerTextureUniform(
      tiles,
      arg => {
        let retval =
          switch tilesTexture^ {
          | Some(texture) => texture
          | None => getNewTexture(arg.gl, "/resources/tiles.jpg")
          };
        tilesTexture := Some(retval);
        retval;
      }
    );
  /* TODO: cube texture */
  let tilesTexture5 = ref(None);
  let registeredSky =
    registerTextureUniform(
      sky,
      arg => {
        let retval =
          switch tilesTexture5^ {
          | Some(texture) => texture
          | None => getNewTexture(arg.gl, "/resources/ypos.jpg")
          };
        tilesTexture5 := Some(retval);
        retval;
      }
    );
  let getUniforms = (textureRef, causticsRef, terrainRef, heightMapRef, isc) => [
    r(u_modelViewMatrix, arg => arg.modelViewMatrix),
    r(u_projectionMatrix, arg => arg.projectionMatrix),
    /* r(sphereCenter, (_) => [|(-0.4), (-0.75), 2.0|]), */
    /* r(sphereCenter, (_) => [|0.0, -0.75, 0.0|]), */
    r(isCaustics, (_) => [|isc|]),
    r(sphereCenter, (_) => [|10.0, 0.0, 10.0|]),
    r(sphereRadius, (_) => [|0.25|]),
    r(u_isAboveWater, (_) => [|1.0|]),
    r(waterHeight, (_) => [|ConfigVars.waterHeight#get()|]),
    r(waveHeight, (_) => [|ConfigVars.waveHeight#get()|]),
    r(
      light,
      (_) => {
        let (x, y, z) = (
          ConfigVars.lightX#get(),
          ConfigVars.lightY#get(),
          ConfigVars.lightZ#get()
        );
        let (x, y, z) = ShaderLib.normalize(x, y, z);
        [|x, y, z|];
      }
    ),
    r(
      eye,
      arg => {
        let (x, y, z) = arg.eye;
        let (x, y, z) = (
          ConfigVars.eyeX#get(),
          ConfigVars.eyeY#get(),
          ConfigVars.eyeZ#get()
        );
        [|x, y, z|];
      }
    ),
    r(u_time, arg => [|arg.time|]),
    r(poolHeight, arg => [|ConfigVars.poolHeight#get()|]),
    registeredTiles,
    registerTextureUniform(terrain, arg =>
      switch terrainRef^ {
      | Some(x) => x
      | None => memGRT(arg.gl, 0.0, 0.0)
      }
    ),
    registerTextureUniform(water, arg =>
      switch textureRef^ {
      | Some(x) => x
      | None => memGRT(arg.gl, 0.0, 0.0)
      }
    ),
    registerTextureUniform(causticTex, arg =>
      switch causticsRef^ {
      | Some(x) => x
      | None => memGRT(arg.gl, 0.0, 0.0)
      }
    ),
    registerTextureUniform(heightMap, arg =>
      switch heightMapRef^ {
      | Some(x) => x
      | None => memGRT(arg.gl, 0.0, 0.0)
      }
    ),
    registeredSky
  ];
  let makeProgramSource =
    Memoize.partialMemoize4(
      (textureRef, causticsRef, terrainRef, heightMapRef) => {
      let uniformBlock =
        getUniforms(textureRef, causticsRef, terrainRef, heightMapRef, -1.0);
      (
        uniformBlock,
        getProgram(uniformBlock, waterVertexShader, waterFragmentShader)
        /*
         {
           ...getProgram(uniformBlock, waterVertexShader, waterFragmentShader),
           fragmentShader: verbatimSource
         }*/
      );
    });
  let makeCausticsProgramSource =
    Memoize.partialMemoize1(textureRef => {
      let uniformBlock =
        getUniforms(textureRef, ref(None), ref(None), ref(None), 1.0);
      (
        uniformBlock,
        getProgram(uniformBlock, causticsVertexShader, causticsFragmentShader)
      );
    });
};

module Water = {
  /*
   * WebGL Water
   * http://madebyevan.com/webgl-water/
   *
   * Copyright 2011 Evan Wallace
   * Released under the MIT license
   */
  /* The data in the texture is (position.y, velocity.y, normal.x, normal.z) */
  /*
   let coord = vec2varying("coord");
   */
  let coord = gl_FragCoord **. xy' / u_resolution;
  let vertexShader =
    body(()
      /*
       coord =@ gl_Vertex **. xy' * f(0.5) + f(0.5);
       gl_Position =@ vec4(gl_Vertex **. xyz' |+| f(1.0));
       */
      =>
        gl_Position
        =@ u_projectionMatrix
        * u_modelViewMatrix
        * vec4(gl_Vertex **. xyz' |+| f(1.0))
      );
  let delta = vec2uniform("delta");
  let updateFragmentShader =
    body(() => {
      /* get vertex info */
      let info = vec4var("info");
      info =@ texture(water, coord);
      /* calculate average neighbor height */
      let dx = vec2var("dx");
      let dy = vec2var("dy");
      dx =@ vec22f(delta **. x', f(0.0));
      dy =@ vec22f(f(0.0), delta **. y');
      let average = floatvar("average");
      average
      =@ (
        texture(water, coord - dx)
        **. r'
        + texture(water, coord - dy)
        **. r'
        + texture(water, coord + dx)
        **. r'
        + texture(water, coord + dy)
        **. r'
      )
      * f(0.25);
      /* change the velocity to move toward the average */
      info **. g' += (average - info **. r') * f(2.0);
      /* attenuate the velocity a little so waves do not last forever */
      /*
       info **. g' *= f(0.9995);
       */
      /* move the vertex along the velocity */
      info **. r' += info **. g' * f(1.0);
      gl_FragColor =@ info;
    });
  /* XXX: debug */
  /*
   gl_FragColor **. rg' =@ gl_FragCoord **. xy' / u_resolution;
   gl_FragColor **. a' =@ f(1.0);
   */
  let normalFragmentShader =
    body(() => {
      /* get vertex info */
      let info = vec4var("info");
      let scale = waveHeight;
      info =@ texture(water, coord) * scale;
      /* update the normal */
      let dx = vec3var("dx");
      let dy = vec3var("dy");
      dx
      =@ vec33f(
           delta **. x',
           texture(water, vec22f(coord **. x' + delta **. x', coord **. y'))
           **. r'
           * scale
           - info
           **. r',
           f(0.0)
         );
      dy
      =@ vec33f(
           f(0.0),
           texture(water, vec22f(coord **. x', coord **. y' + delta **. y'))
           **. r'
           * scale
           - info
           **. r',
           delta **. y'
         );
      info **. ba' =@ normalize(cross(dy, dx)) **. xz';
      gl_FragColor =@ info;
    });
  let r = registerUniform;
  let getUniforms = textureRef => [
    r(u_modelViewMatrix, arg => arg.modelViewMatrix),
    r(u_projectionMatrix, arg => arg.projectionMatrix),
    r(u_resolution, arg =>
      [|float_of_int(arg.width), float_of_int(arg.height)|]
    ),
    r(delta, arg =>
      [|1.0 /. float_of_int(arg.width), 1.0 /. float_of_int(arg.height)|]
    ),
    r(waveHeight, (_) => [|ConfigVars.waveHeight#get()|]),
    registerTextureUniform(water, arg =>
      switch textureRef^ {
      | Some(x) => x
      | None => memGRT(arg.gl, 0.0, 0.0)
      }
    )
  ];
  let makeProgramSource = textureRef => {
    let uniformBlock = getUniforms(textureRef);
    (
      uniformBlock,
      getProgram(uniformBlock, vertexShader, updateFragmentShader)
    );
  };
  let makeNormalProgramSource = textureRef => {
    let uniformBlock = getUniforms(textureRef);
    (
      uniformBlock,
      getProgram(uniformBlock, vertexShader, normalFragmentShader)
    );
  };
};
