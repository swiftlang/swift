#!/usr/bin/env python
# update-dependencies.py - Fake build for dependency analysis -*- python -*-
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
# Simulates a Swift compilation for the purposes of dependency analysis.
# That means this has two tasks:
#
# 1. Update the main output of the compilation job.
# 2. Update the associated dependencies file, in case anything changed.
#
# Since we only care about the timestamp of the output, the script just makes
# sure the file's mtime is set to $NOW, creating it if it doesn't exist.
# We don't actually care about the content of the input file, either, so we
# actually stick *the new dependencies* in the input file, and copy that over
# the old dependencies (if present).
#
# If invoked in non-primary-file mode, it only creates the output file.
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import os
import shutil
import sys

assert sys.argv[1] == '-frontend'

# NB: The bitcode options automatically specify a -primary-file, even in cases
#     where we do not wish to use a dependencies file in the test.
if '-primary-file' in sys.argv \
        and '-embed-bitcode' not in sys.argv and '-emit-bc' not in sys.argv:
    primaryFile = sys.argv[sys.argv.index('-primary-file') + 1]
    depsFile = sys.argv[sys.argv.index(
        '-emit-reference-dependencies-path') + 1]

    # Replace the dependencies file with the input file.
    shutil.copyfile(primaryFile, depsFile)
else:
    primaryFile = None

outputFile = sys.argv[sys.argv.index('-o') + 1]

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
with open(outputFile, 'a'):
    os.utime(outputFile, None)

if primaryFile:
    print("Handled", os.path.basename(primaryFile))
else:
    print("Produced", os.path.basename(outputFile))
