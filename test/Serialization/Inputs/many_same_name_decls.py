#!/usr/bin/env python3
# Emit `count` structs that each declare a nested type `N` and a `==` operator
# method. With enough structs, the module-wide operator-method and nested-type
# table entries for `==` and `N` push their dataLength past 16 bits.

import sys

count = int(sys.argv[1])
for i in range(count):
    print("public struct S%d {" % i)
    print("  public struct N {}")
    print("  public static func == (a: S%d, b: S%d) -> Bool { return true }"
          % (i, i))
    print("}")
