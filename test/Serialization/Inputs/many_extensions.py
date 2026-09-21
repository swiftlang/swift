#!/usr/bin/env python3
# Emit a nested struct with `count` extensions, each adding a distinct method.
# This is Used to push one type's serialized extension-table entry past the 16-bit
# dataLength limit.

import sys

count = int(sys.argv[1])
print("public struct Outer { public struct Inner {} }")
for i in range(1, count + 1):
    print("extension Outer.Inner { public func f%d() -> Int { return %d } }" % (i, i))
