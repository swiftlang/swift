#!/usr/bin/env python
# fake-build-for-bitcode.py - Fake build with -embed-bitcode -*- python -*-
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
# Emulates the frontend of an -embed-bitcode job. That means we have to handle
# -emit-bc and -c actions.
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import os
import sys

assert sys.argv[1] == '-frontend'

primaryFile = sys.argv[sys.argv.index('-primary-file') + 1]
outputFile = sys.argv[sys.argv.index('-o') + 1]

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
with open(outputFile, 'a'):
    os.utime(outputFile, None)

if '-emit-bc' in sys.argv:
    print("Handled", os.path.basename(primaryFile))
elif '-c' in sys.argv:
    print("Produced", os.path.basename(outputFile))
else:
    assert False, "unknown action"
