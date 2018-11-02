#!/usr/bin/env python
#
# make-old.py - /bin/touch that writes a constant "old" timestamp.
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
# Like /bin/touch, but writes a constant "old" timestamp.
#
# ----------------------------------------------------------------------------

import os
import sys

OLD = 1390550700 # 2014-01-24T08:05:00+00:00
for f in sys.argv[1:]:
    os.utime(f, (OLD, OLD))
