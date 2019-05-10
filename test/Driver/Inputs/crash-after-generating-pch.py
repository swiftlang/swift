#!/usr/bin/env python
# crash.py - Sends SIGKILL to self. -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import platform
import signal
import sys

assert sys.argv[1] == '-frontend'

if '-emit-pch' not in sys.argv:
    if platform.system() == 'Windows':
        exit(-2)
    else:
        os.kill(os.getpid(), signal.SIGKILL)

outputFile = sys.argv[sys.argv.index('-o') + 1]

# Update the output file mtime, or create it if necessary.
# From http://stackoverflow.com/a/1160227.
with open(outputFile, 'a'):
    os.utime(outputFile, None)
