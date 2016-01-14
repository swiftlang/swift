#!/usr/bin/env python
# check-filelist-abc.py - Fake build to test driver-produced -filelists.
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

filelistFile = sys.argv[sys.argv.index('-filelist') + 1]

with open(filelistFile, 'r') as f:
  lines = f.readlines()
  assert lines[0].endswith("/a.swift\n")
  assert lines[1].endswith("/b.swift\n")
  assert lines[2].endswith("/c.swift\n")

if primaryFile:
  print("Handled", os.path.basename(primaryFile))
else:
  print("Handled all")
