#!/usr/bin/env python
import re
import sys
import glob

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
