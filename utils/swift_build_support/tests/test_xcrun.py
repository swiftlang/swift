# test_xcrun.py - Unit tests for swift_build_support.xcrun -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import platform
import unittest

from swift_build_support import xcrun


@unittest.skipUnless(platform.system() == 'Darwin',
                     'xcrun is available in Darwin platform only')
class XCRunTestCase(unittest.TestCase):
    def test_find(self):
        # Unknown tool
        self.assertIsNone(xcrun.find('a-tool-that-isnt-on-osx',
                                     sdk='macosx',
                                     toolchain='default'))

        # Available tool
        self.assertTrue(xcrun.find('clang',
                                   sdk='macosx',
                                   toolchain='default').endswith('/clang'))

    def test_sdk_path(self):
        # Unknown SDK
        self.assertIsNone(xcrun.sdk_path('not-a-sdk'))

        # Available SDK
        self.assertIsNotNone(xcrun.sdk_path('macosx'))


if __name__ == '__main__':
    unittest.main()
