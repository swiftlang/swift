#!/usr/bin/env python
# fake-build-for-whole-module.py - Optimized fake build -*- python -*-
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
#
# Emulates the frontend of a -whole-module-optimization compilation.
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import os
import sys

assert sys.argv[1] == '-frontend'
assert '-primary-file' not in sys.argv

outputFile = sys.argv[sys.argv.index('-o') + 1]

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
with open(outputFile, 'a'):
    os.utime(outputFile, None)

print("Produced", os.path.basename(outputFile))
