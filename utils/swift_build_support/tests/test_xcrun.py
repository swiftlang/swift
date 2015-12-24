# test_xcrun.py - Unit tests for swift_build_support.xcrun -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import platform
import unittest

from swift_build_support import xcrun


class FindTestCase(unittest.TestCase):
    def setUp(self):
        if platform.system() != 'Darwin':
            self.skipTest('XCRun tests should only be run on OS X')

    def test_when_tool_not_found_returns_none(self):
        self.assertIsNone(xcrun.find(
            toolchain='default', tool='a-tool-that-isnt-on-osx'))

    def test_when_tool_found_returns_path(self):
        self.assertTrue(xcrun.find(
            toolchain='default', tool='clang').endswith('/clang'))


if __name__ == '__main__':
    unittest.main()
