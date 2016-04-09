# tests/test_defaults.py ----------------------------------------*- python -*-
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

import unittest

from build_script import defaults


class ShellTestCase(unittest.TestCase):
    def test_default_exists(self):

        assertTrue = self.assertTrue

        # Configurable
        assertTrue(hasattr(defaults, 'SWIFT_USER_VISIBLE_VERSION'))
        assertTrue(hasattr(defaults, 'CLANG_USER_VISIBLE_VERSION'))
        assertTrue(hasattr(defaults, 'SWIFT_ANALYZE_CODE_COVERAGE'))
        assertTrue(hasattr(defaults, 'DARWIN_XCRUN_TOOLCHAIN'))
        assertTrue(hasattr(defaults, 'DARWIN_DEPLOYMENT_VERSION_OSX'))
        assertTrue(hasattr(defaults, 'DARWIN_DEPLOYMENT_VERSION_IOS'))
        assertTrue(hasattr(defaults, 'DARWIN_DEPLOYMENT_VERSION_TVOS'))
        assertTrue(hasattr(defaults, 'DARWIN_DEPLOYMENT_VERSION_WATCHOS'))
        assertTrue(hasattr(defaults, 'UNIX_INSTALL_PREFIX'))
        assertTrue(hasattr(defaults, 'DARWIN_INSTALL_PREFIX'))

        # Constants
        assertTrue(hasattr(defaults, 'LLVM_TARGETS_TO_BUILD'))
