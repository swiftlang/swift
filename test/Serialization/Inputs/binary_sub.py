#!/usr/bin/env python

import sys

(_, old, new) = sys.argv
assert(len(old) == len(new))

if sys.version_info[0] == 2:
    stdin = sys.stdin
    stdout = sys.stdout
else:
    stdin = sys.stdin.buffer
    stdout = sys.stdout.buffer
    old = old.encode()
    new = new.encode()

data = stdin.read()
stdout.write(data.replace(old, new))
