#!/usr/bin/env python
# modify-non-primary-files.py - Fake build while modifying files -*- python -*-
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
# modify-non-primary-files.py simulates a build where the user is modifying the
# source files during compilation.
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import os
import sys

assert sys.argv[1] == '-frontend'

if '-primary-file' in sys.argv:
    primaryFileIndex = sys.argv.index('-primary-file') + 1
    primaryFile = sys.argv[primaryFileIndex]

    # Modify all files after the primary file.
    # Ideally this would modify every non-primary file, but that's harder to
    # infer without actually parsing the arguments.
    for file in sys.argv[primaryFileIndex + 1:]:
        if file.startswith('-'):
            break
        os.utime(file, None)

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
