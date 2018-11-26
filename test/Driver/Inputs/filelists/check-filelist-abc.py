#!/usr/bin/env python
# check-filelist-abc.py - Fake build to test driver-produced -filelists.
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

import os
import sys

assert sys.argv[1] == '-frontend'

if '-primary-file' in sys.argv:
    primaryFile = sys.argv[sys.argv.index('-primary-file') + 1]
else:
    primaryFile = None

if primaryFile and primaryFile.endswith(".bc"):
    sys.exit()

filelistFile = sys.argv[sys.argv.index('-filelist') + 1]

with open(filelistFile, 'r') as f:
    lines = f.readlines()
    assert(lines[0].endswith("/a.swift\n") or
           lines[0].endswith("/a.swiftmodule\n"))
    assert(lines[1].endswith("/b.swift\n") or
           lines[1].endswith("/b.swiftmodule\n"))
    assert(lines[2].endswith("/c.swift\n") or
           lines[2].endswith("/c.swiftmodule\n"))

if primaryFile:
    print("Command-line primary", os.path.basename(primaryFile))

if '-primary-filelist' in sys.argv:
    primaryFilelistFile = sys.argv[sys.argv.index('-primary-filelist') + 1]
    with open(primaryFilelistFile, 'r') as f:
        lines = f.readlines()
        assert(len(lines) == 1)
        print("Handled", os.path.basename(lines[0]).rstrip())
elif lines[0].endswith(".swiftmodule\n"):
    print("Handled modules")
else:
    print("Handled all")


if '-supplementary-output-file-map' in sys.argv:
    supplementaryOutputMapFile = \
        sys.argv[sys.argv.index('-supplementary-output-file-map') + 1]
    with open(supplementaryOutputMapFile, 'r') as f:
        lines = f.readlines()
        for line in lines:
            print("Supplementary", line.rstrip())

if '-num-threads' in sys.argv:
    outputListFile = sys.argv[sys.argv.index('-output-filelist') + 1]
    with open(outputListFile, 'r') as f:
        lines = f.readlines()
        assert(lines[0].endswith("/a.o\n") or lines[0].endswith("/a.bc\n"))
        assert(lines[1].endswith("/b.o\n") or lines[1].endswith("/b.bc\n"))
        assert(lines[2].endswith("/c.o\n") or lines[2].endswith("/c.bc\n"))
    print("...with output!")
