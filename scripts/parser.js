Error.stackTraceLimit = Infinity;

var fs = require('fs');
var cycle = require('./cycle.js');
var glslify = require('glslify');
var GLSL = require('glsl-transpiler');

var source = glslify(process.argv[2]);

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
    /*
    if (node.type == "expr") {
      var leftParen = false;
      for (var i = node.position; i < node.position + 1; i++) {
        var node = positions[i].node;
        var token = positions[i].token;
        if (node.data.match(/(/) || token.data.match(/(/)) {
          leftParen = true;
        }
      }
      if (leftParen) {
        console.log("leftParen");
        insertToken({
          position: node.maxPos,
          data: ")"
        });
      }
    }*/
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
