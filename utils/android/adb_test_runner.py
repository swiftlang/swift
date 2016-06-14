#!/usr/bin/env python
# adb_test_runner.py - Calls adb_test_runner.main -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import sys

from adb_test_runner.main import main


if __name__ == '__main__':
    sys.exit(main())
