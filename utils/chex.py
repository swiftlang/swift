#!/usr/bin/env python
# Check HEX -- a stupid filter that allows hexadecimal literals to be checked
# for in LLVM IR FileCheck tests. It works by replacing occurrences of
# strings matching the regex /< (i[0-9]+) \s+ (0x[0-9A-Fa-f]+) >/x with the
# decimal literal equivalent that would really appear in printed LLVM IR.

from __future__ import print_function

import re
import sys

hex = re.compile(r"""<(i([0-9]+)\s+)0x([0-9A-Fa-f_]+)>""")


def hexReplace(match):
    # Integer type is match group 1
    ty = match.group(1)
    # Integer bit width is match group 2
    bits = int(match.group(2))
    # Hex value is match group 3
    value = int(match.group(3).replace("_", ""), base=16)
    # LLVM prints the decimal value as if it's two's-complement signed in
    # the given bitwidth, so the printed value will be negative if
    # greater than 2^(bits - 1)
    if value >= (1 << (bits - 1)):
        value -= 1 << bits
    return ty + str(value)


for line in sys.stdin:
    print(re.sub(hex, hexReplace, line), end="")
