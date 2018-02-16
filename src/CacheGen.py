#!/usr/bin/env python
import re
import sys
import glob
import subprocess

def getTypeOut(fname):
    bsc = subprocess.Popen(["../node_modules/.bin/bsc", fname], shell=False, stdout=subprocess.PIPE)
    refmt = subprocess.Popen(["/usr/bin/refmt", "--parse=ml", "-i", "true", "-w", "10000"], shell=False, stdin=bsc.stdout, stdout=subprocess.PIPE)
    data = refmt.communicate()[0]
    ret = refmt.returncode
    ret2 = bsc.returncode
    if ret != 0:
        print >> sys.stderr, 'refmt error on ' + fname
    return data

def getOut(fname):
    refmt = subprocess.Popen(["/usr/bin/refmt", "-w", "10000", fname], shell=False, stdout=subprocess.PIPE)
    data = refmt.communicate()[0]
    ret = refmt.returncode
    if ret != 0:
        print >> sys.stderr, 'refmt error on ' + fname
    return data

def getFunArgs(module, data, fname):
    for line in data.splitlines():
        if line.startswith('let %s' % fname):
            args = line.split(" =>")[0].split(" =")[1].strip()
            if '_' in args:
                continue
            if not '(' in args:
                args = '(' + args + ')'
            return args
    print >> sys.stderr, "couldn't get args of " + module + "." + fname
    return None

def fmtModule(module, content):
    return '''
module {m} = {{
{c}
}};
'''.format(m=module,c=content)

def fmtCacheFun(module, funName, argnames):
    return '''
    let {f}Cache = ref(None);
    let {f} = {a} => {{
        let args = {a};
        switch {f}Cache^ {{
        | Some((retval, savedArgs)) when savedArgs == args => retval
        | _ =>
          let retval = {m}.{f}{a};
          {f}Cache := Some((retval, args));
          retval;
        }};
    }};
'''.format(m=module, f=funName, a=argnames)

def isBalanced(x):
    left = 0
    right = 0
    for c in x:
        if c == '(':
            left += 1
        elif c == ')':
            right += 1
    return left == right

#print getOut("../lib/bs/Rts.cmi");
def processModule(module):
    typeData = getTypeOut("../lib/bs/src/%s-Rts.cmi" % module);
    data = getOut("%s.re" % module)
    lines = typeData.splitlines()
    content = ''
    for line in lines:
        if line.startswith('let') and '=>' in line:
            tokens = line.split(" ")
            funName = tokens[1].rstrip(":")
            l = line.split('=>')
            good = False
            for i in range(1, len(l)):
                x, y = "".join(l[:i]), "".join(l[i:])
                #print >> sys.stderr, (l, i, x, y)
                if isBalanced(x) and isBalanced(y):
                    good = True
                    break
            if not good:
                print >> sys.stderr, "probably not a function, skipping: " + funName
                continue
            argnames = getFunArgs(module, data, funName)
            #retType = line.split('=>');
            if argnames != None:
                content += fmtCacheFun(module, funName, argnames)
    if content != '':
        fd = file(module + 'Cache.re', 'w')
        #fd.write(fmtModule(module, content))
        fd.write(content)
        fd.close()

for fname in glob.glob('*.re'):
    module = fname[:-3]
    if 'Cache' in module:
        continue
    processModule(module)

sys.exit()

for fname in glob.glob('*.re'):
    fd = file(fname)
    data = fd.read()
    fd.close()
    lines = data.splitlines()
    for i, line in enumerate(lines):
        match = re.search(r'^let [a-zA-Z0-9]+ = \([^)]+\) => ', line)
        if match:
            for j in range(i, len(lines)):
                line2 = lines[j]
                if line.strip() == '':
                    break
                print line2
