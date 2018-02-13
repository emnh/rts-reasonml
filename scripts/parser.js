Error.stackTraceLimit = Infinity;

var fs = require('fs');
var cycle = require('./cycle.js');
var glslify = require('glslify');
var GLSL = require('glsl-transpiler');
var Descriptor = require('glsl-transpiler/lib/descriptor');

// wrap literals
var oldLiteral = GLSL.prototype.transforms.literal;
GLSL.prototype.transforms.literal = function(node) {
  var old = oldLiteral(node);
  var value = old;
  if (old.type == 'float') {
    var s = new String('f(' + old + ')');
    value = Descriptor(s);
  }
  return value;
};

// replace dot operator
// GLSL.prototype.transforms.oldOperator = GLSL.prototype.transforms.operator;
/*
var compile;
GLSL.prototype.transforms.operator = function(node) {
  //access operators - expand to arrays
  if (node.data === '.') {
    var identNode = node.children[0];
    var ident = this.process(identNode);
    var type = ident.type;
    var prop = node.children[1].data;

    //ab.xyz for example
    if (/^[xyzwstpdrgba]{1,4}$/.test(prop)) {
      // prop = prop.toUpperCase();
      var ret = this.unswizzle(node);
      var ret2 = Descriptor(`${prop}(${ident})`, ret); 
      console.log("ret", ret2);
      return ret2;
    }

    return Descriptor(`${ident}.${prop}`, {
      type: type
    });
  }

  throw Error('Unknown operator ' + node.data);

  return Descriptor(null);
}*/

GLSL.prototype.oldUnswizzle = GLSL.prototype.unswizzle;
GLSL.prototype.unswizzle = function(node) {
  var identNode = node.children[0];
  var ident = this.process(identNode);
  var type = ident.type;
  var prop = node.children[1].data;

  //ab.xyz for example
  if (/^[xyzwstpdrgba]{1,4}$/.test(prop)) {
    // prop = prop.toUpperCase();
    var ret = this.oldUnswizzle(node);
    var ret2 = Descriptor(`${prop}(${ident})`, ret); 
    //console.log("ret", ret2);
    return ret2;
  }

  return Descriptor(`${ident}.${prop}`, {
    type: type
  });
};

GLSL.prototype.transforms.assign = function(node) {
  var result = '';
  var operator = node.data;

  var right = this.process(node.children[1]);
  if (node.children[0].type === 'identifier') {
    var left = Descriptor(node.children[0].data, {
      type: right.type,
      optimize: false,
      complexity: 0
    });
  }
  else {
    var left = this.process(node.children[0]);
  }

  var target = left;
  var isSwizzle = node.children[0].type === 'operator' && /^[xyzwstpdrgba]{1,4}$/.test(node.children[0].children[1].data);

  isSwizzle = false;

  //a *= b.x
  if (!isSwizzle && this.types[right.type].length == 1 && this.types[target.type].length == 1) {
    return Descriptor(`${target} ${operator} ${right}`, {
      type: right.type,
      complexity: target.complexity + 1 + right.complexity
    });
  }

  //FIXME: left can be a structure property set a.prop

  //in cases of setting swizzle - we gotta drop left unswizzle to the right
  if (isSwizzle) {
    var positions = this.swizzlePositions(node.children[0].children[1].data);
    var len = this.types[this.process(node.children[0].children[0]).type].length;
    var ids = Array(len).fill('null');

    for (var i = 0; i < positions.length; i++) {
      ids[positions[i]] = i;
    }

    target = Descriptor(node.children[0].children[0].data, {
      type: right.type,
      optimize: false
    });

    //a.wy *= a.zx →
    //a = [null, 1, null, 0].map(function (idx, i) {
    //	return idx == null ? gl_position[i] : this[idx];
    //}, a.wy * a.zx)
    if (positions.length > 1) {
      //*=
      if (operator.length > 1) {
        var subOperator = operator.slice(0, -1);
        right = this.processOperation(this.unswizzle(node.children[0]), right, subOperator);
        right = this.optimizeDescriptor(right);
      }

      var comps = Array(len);
      for (var i = 0; i < len; i++) {
        comps[i] = Descriptor(`${target}[${i}]`, {
          type: 'float',
          complexity: 1
        });
      }
      for (var i = 0; i < positions.length; i++) {
        comps[positions[i]] = right.components[i];
      }

      right = Descriptor(
        `[${ids.join(', ')}].map(function (idx, i) { return idx == null ? ${target}[i] : this[idx]; }, ${right})`, {
          type: right.type,
          complexity: len*4 + right.complexity,
          include: right.include,
          components: comps
      });
      right = this.optimizeDescriptor(right);

      return Descriptor(`${target} =@ ${right}`, {
        type: right.type,
        optimize: false,
        include: right.include
      });
    }
    //a.x *= b → a[0] *= b
    else {
      return Descriptor(`${target}[${positions[0]}] ${operator} ${right}`, {
        type: right.type,
        optimize: false
      });
    }
  }

  //`a *= x` → `a = a * x`
  else if (operator.length > 1) {
    var subOperator = operator.slice(0, -1);
    right = this.processOperation(left, right, subOperator);
    right = this.optimizeDescriptor(right);
  }

  //simple assign, =
  return Descriptor(`${target} =@ ${right}`, {
    type: right.type,
    complexity: 1
  });
};

var source = glslify('./source.glsl');
var options = {
  optimize: true
};

var parsed = [];
GLSL.prototype.oldProcess = GLSL.prototype.process;
GLSL.prototype.process = function (node, arg) {
  parsed.push(node);
  return this.oldProcess(node, arg);
}
compile = GLSL(options);

//compile source code
var result = compile(source);

//get collected info
var compiler = compile.compiler;
compiler.attributes;
compiler.uniforms;
compiler.varyings;
compiler.structs;
compiler.functions;
compiler.scopes;

parsed = parsed[0];
var positions = [];
var Letter = function() {};
for (var i = 0; i < source.length; i++) {
  var letter = new Letter();
  letter.node = undefined;
  letter.letter = source[i];
  letter.inserts = [];
  positions[i] = letter;
}

var whitePass = false;
var insertPass = false;

var path = [];

var seen;

function insertToken(token) {
  var pos = token.position;
  if (pos !== undefined) {
    positions[pos].inserts.push(token);
  }
}

function processToken(isNode, token) {
  var pos = token.position;
  if (pos !== undefined) {
    var length = token.data.length;
    for (var i = 0; i < length; i++) {
      if (isNode) {
        if (positions[pos + i].node === undefined) {
          positions[pos + i].node = token;
        }
      } else {
        if (positions[pos + i].token === undefined) {
          positions[pos + i].token = token;
        }
      }
    }
    if (!isNaN(length) && length > 0) {
      return pos + length;
    } else {
      return 0;
    }
  }
  return 0;
}

var gid = 0;
function processNode(node) {
  if (seen[node.gid] === true) {
    return;
  };
  node.gid = gid++;
  node.maxPos = 0;
  seen[node.gid] = true;
  var ns = [node.data];
  if (node.token !== undefined) {
    ns.push(node.token.data);
  }
  if (node.tokens !== undefined) {
    node.tokens.map((tk) => ns.push(tk.data));
  }
  path.push(ns.join(":") + "(" + node.type + ")");
  // console.log("path", path.join("/"));

  if (node.children !== undefined) {
    node.children.map(processNode);
    node.maxPos =
      Math.max(node.maxPos,
        node.children.map(x => x.maxPos).reduce((x, y) => Math.max(x, y), 0));
  }

  node.maxPos = Math.max(node.maxPos, processToken(false, node.token));
  if (node.tokens !== undefined) {
    node.maxPos = 
      Math.max(node.maxPos,
        node.tokens
          .map((x) => processToken(false, x))
          .reduce((x, y) => Math.max(x, y), 0));
  }

  node.maxPos = Math.max(node.maxPos, processToken(true, node));

  if (whitePass && node.token.preceding !== undefined) {
    node.token.preceding.map((x) => processToken(false, x));
  }

  if (insertPass) {
    if (node.type == "stmt" || node.type == "decl") {
      insertToken({
        position: node.maxPos,
        data: ";"
      });
    }
    if (node.type == "expr") {
      var leftParen = false;
      for (var i = node.position; i < node.maxPos; i++) {
        var node = positions[i].node;
        var token = positions[i].token;
        if (node.data.match(/(/) || token.data.match(/(/)) {
          leftParen = true;
        }
      }
      if (leftParen) {
        insertToken({
          position: node.maxPos,
          data: ")"
        });
      }
    }
  }

  path.pop();
}
seen = {};
processNode(parsed);
whitePass = true;
seen = {};
processNode(parsed);
insertPass = true;
seen = {};
processNode(parsed);

var s = "";
var oldNode = undefined;
for (var i = 0; i < positions.length; i++) {
  var node = positions[i].node || positions[i].token;
  if (oldNode === undefined || oldNode != node) {
    if (node !== undefined) {
      var data = "";
      if (node.type == "whitespace" ||
          node.token === undefined ||
          node.token.data === undefined) {
        data = node.data;
      } else {
        data = node.token.data;
      }
      s += data;
    }
    oldNode = node;
  }
  if (node === undefined) {
    var insert = positions[i].inserts.map(x => x.data).join("");
    if (!s.endsWith(insert)) {
      s += insert;
    }
  }
}
console.log(s);

/*
var dec = JSON.decycle(parsed);
var result = JSON.stringify(dec, null, 2);
fs.writeFileSync("parsed.json", result);
*/

//clean collected info
compiler.reset();
