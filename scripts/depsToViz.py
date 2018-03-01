#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: ft=python ts=4 sw=4 sts=4 et fenc=utf-8
# Original author: "Eivind Magnus Hvidevold" <hvidevold@gmail.com>
# License: GNU GPLv3 at http://www.gnu.org/licenses/gpl.html

'''
'''

import os
import sys
import re
import glob

def f(x):
    return x.replace("src/", "").replace("-Rts.cmj","").replace("-Rts.cmi", "")

def main():
    'entry point'
    print "digraph G {"
    for fname in glob.glob("*.d"):
        data = file(fname).read()
        src, dests = data.split(":")
        src = f(src).strip()
        dests = dests.split(" ")
        dests = [f(d) for d in dests if d != ""]
        for d in dests:
            print '"%s"' % src, "->", '"%s"' % d, ";"
    print "}"

if __name__ == '__main__':
    main()

