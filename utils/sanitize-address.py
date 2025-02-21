#!/usr/bin/env python3

# This source file is part of the Swift.org open source project
#
# Copyright (c) 2025 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

"""
Replaces hexadecimal addresses (e.g., '0x12f42') with '<addr N>', where N is a
sequence number. The same address gets the same replacement.
"""

import re
import sys

pattern = re.compile(r'0x[0-9a-zA-Z]+')

seen = {}


def replacement(match):
    addr = match.group()
    if addr not in seen:
        seen[addr] = "<addr " + str(len(seen)) + ">"

    return seen[addr]


for line in sys.stdin:
    print(pattern.sub(replacement, line), end="")
