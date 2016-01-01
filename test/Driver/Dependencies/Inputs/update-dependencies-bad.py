#!/usr/bin/env python
# update-dependencies-bad.py - Fails on bad.swift -*- python -*-
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
#
# Fails if the input file is named "bad.swift"; otherwise dispatches to
# update-dependencies.py.
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import os
import sys

assert sys.argv[1] == '-frontend'

primaryFile = sys.argv[sys.argv.index('-primary-file') + 1]

if os.path.basename(primaryFile) == 'bad.swift':
    print("Handled", os.path.basename(primaryFile))
    exit(1)

dir = os.path.dirname(os.path.abspath(__file__))
execfile(os.path.join(dir, "update-dependencies.py"))
