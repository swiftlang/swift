#!/usr/bin/env python
#
# check-is-old.py - a more-legible way to read a timestamp test than test(1)
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
#
# Compares the mtime of the provided files to a constant "old" reference time.
# Fails if any file has mtime not-equal to old.
#
# ----------------------------------------------------------------------------

import os
import sys

OLD = 1390550700 # 2014-01-24T08:05:00+00:00
for f in sys.argv[1:]:
    if os.stat(f).st_mtime != OLD:
        print("%s is not old!" % f)
        exit(1)


