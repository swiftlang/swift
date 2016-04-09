# build_script/swift_utils.py -----------------------------------*- python -*-
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
"""
Provides utility scripts path in swift/utils
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import sys
import os.path

from . import env


class SwiftUtils(object):
    def __init__(self):
        self.basedir = os.path.join(env.SWIFT_SOURCE_ROOT, 'swift', 'utils')

    def __call__(self, name):
        import os.path  # Because callable module hack hides module global.
        return os.path.join(self.basedir, name)

# This is a callable module.
# http://stackoverflow.com/a/1060872/3804019
sys.modules[__name__] = SwiftUtils()
