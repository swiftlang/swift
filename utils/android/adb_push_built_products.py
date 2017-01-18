#!/usr/bin/env python
# adb_push_build_products.py - Push libraries to Android device -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import sys

from adb_push_built_products.main import main


if __name__ == '__main__':
    sys.exit(main())
