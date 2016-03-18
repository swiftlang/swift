# -*- python -*-
# test_toolchain.py - Unit tests for swift_build_support.toolchain
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import unittest

from swift_build_support.toolchain import host_clang, host_toolchain


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

    def test_found_executables_match(self):
        # Test that the raw incovation of _first_common_executables
        # either returns None or matching paths.
        suffixes = ['', '-3.8', '-3.7', '-3.6']
        toolchain = host_toolchain(suffixes=suffixes)
        self.assertTrue(len(toolchain.tools) == 2)

        exec_names = {'foo': 'a-tool-that-does-not-exist'}
        toolchain = host_toolchain(tools=exec_names,
                                   suffixes=suffixes)
        self.assertIsNone(toolchain)


if __name__ == '__main__':
    unittest.main()
