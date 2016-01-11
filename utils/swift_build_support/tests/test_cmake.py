# test_cmake.py - Unit tests for swift_build_support.cmake -*- python -*-
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

from swift_build_support.cmake import host_cmake


class HostCMakeTestCase(unittest.TestCase):
    def test_cmake_available_on_this_platform(self):
        # Test that CMake is installed on this platform, as a means of
        # testing host_cmake().
        cmake = host_cmake(xcrun_toolchain='default')
        self.assertEqual(os.path.split(cmake)[-1], 'cmake')


if __name__ == '__main__':
    unittest.main()
