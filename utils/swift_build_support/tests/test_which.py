# test_which.py - Unit tests for swift_build_support.which -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import unittest

from swift_build_support.which import which


class WhichTestCase(unittest.TestCase):
    def test_when_cmd_not_found_returns_none(self):
        self.assertIsNone(which('a-tool-that-doesnt-exist'))

    def test_when_cmd_found_returns_path(self):
        self.assertEqual(os.path.split(which('ls'))[-1], 'ls')


if __name__ == '__main__':
    unittest.main()
