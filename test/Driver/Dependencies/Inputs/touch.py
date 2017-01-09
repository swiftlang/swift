#!/usr/bin/env python
# touch.py - /bin/touch that writes the LLVM epoch -*- python -*-
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
# Like /bin/touch, but takes a time using the system_clock epoch.
#
# ----------------------------------------------------------------------------

import os
import sys

assert len(sys.argv) >= 2
timeVal = int(sys.argv[1])

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
for outputFile in sys.argv[1:]:
    with open(outputFile, 'a'):
        os.utime(outputFile, (timeVal, timeVal))
