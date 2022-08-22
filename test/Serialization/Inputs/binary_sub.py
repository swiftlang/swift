#!/usr/bin/env python3

import sys

(_, old, new) = sys.argv
assert len(old) == len(new)

if sys.version_info[0] < 3:
    data = sys.stdin.read()
    sys.stdout.write(data.replace(old, new))
else:
    data = sys.stdin.buffer.read()
    sys.stdout.buffer.write(data.replace(old.encode('utf-8'), new.encode('utf-8')))
