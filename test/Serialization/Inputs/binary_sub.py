#!/usr/bin/env python2

import sys

(_, old, new) = sys.argv
assert(len(old) == len(new))

data = sys.stdin.read()
sys.stdout.write(data.replace(old, new))
