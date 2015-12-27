# test_clang.py - Unit tests for swift_build_support.clang -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import unittest

from swift_build_support.clang import host_clang


class HostClangTestCase(unittest.TestCase):
    def test_clang_available_on_this_platform(self):
        # Test that Clang is installed on this platform, as a means of
        # testing host_clang().
        clang = host_clang(xcrun_toolchain='default')

        # The CC and CXX from host_clang() should be of the form
        # 'path/to/clang', where 'clang' may have a trailing version
        # number.
        self.assertTrue(os.path.split(clang.cc)[-1].startswith('clang'))
        self.assertTrue(os.path.split(clang.cxx)[-1].startswith('clang++'))


if __name__ == '__main__':
    unittest.main()
