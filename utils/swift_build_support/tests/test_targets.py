# test_targets.py - Unit tests for swift_build_support.targets -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import unittest

from swift_build_support.targets import host_target


class HostTargetTestCase(unittest.TestCase):
    def test_is_not_none_on_this_platform(self):
        self.assertIsNotNone(host_target())


if __name__ == '__main__':
    unittest.main()
