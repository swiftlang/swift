#!/usr/bin/env python
# fake-ld.py - Fake Darwin linker to test driver-produced -filelists.
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import sys

filelistFile = sys.argv[sys.argv.index('-filelist') + 1]

with open(filelistFile, 'r') as f:
    lines = f.readlines()
    assert lines[0].endswith("/a.o\n")
    assert lines[1].endswith("/b.o\n")
    assert lines[2].endswith("/c.o\n")

print("Handled link")
