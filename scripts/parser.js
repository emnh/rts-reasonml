Error.stackTraceLimit = Infinity;

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
var Letter = function() {
};
for (var i = 0; i < source.length; i++) {
  var letter = new Letter();
  letter.letter = source[i];
  letter.node = undefined;
  positions[i] = letter;
}

function processNode(node) {
  var arrays = node.children.map(processNode);
  var merged = [].concat.apply([], arrays);
  /*
  if (node.token.preceding !== undefined) {
    var preceding = node.token.preceding.map(function(x) { 
      var pos = x.position;
      var length = x.data.length;
      var printIt = true;
      for (var i = 0; i < length; i++) {
        if (positions[pos + i].node !== undefined) {
          printIt = false;
        } else {
          positions[pos + i] = x.data[i];
        }
      }
      return printIt ? x.data : "";
    });
    preceding = [].concat.apply([], preceding);
    merged.unshift(preceding);
  }
  */
  var pos = node.token.position;
  var length = node.token.data.length;
  var printIt = true;
  for (var i = 0; i < length; i++) {
    if (positions[pos + i].node !== undefined) {
      printIt = false;
    } else {
      positions[pos + i] = node;
    }
  }
  if (printIt) {
    merged.unshift(node.token.data);
  }
  switch (node.type) {
    case "stmt":
      merged.push(";");
      break;
  }
  return(merged);
}
var tokens = processNode(parsed);

var s = "";
for (var i = 0; i < Object.keys(positions).length; i++) {
  s += positions[i];
}
console.log(s);

//var dec = JSON.decycle(parsed[0]);
//console.log(JSON.stringify(dec));
//console.log(result);

//clean collected info
compiler.reset();
