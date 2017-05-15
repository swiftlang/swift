# tests/utils/test_which.py -------------------------------------*- python -*-
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
import os.path

from build_script.utils import which


class WhichTestCase(unittest.TestCase):

    def test_when_cmd_not_found_returns_none(self):
        result = which('a-tool-that-doesnt-exist')
        self.assertIsNone(result)

    def test_when_cmd_found_returns_absolute_path(self):
        result = which('ls')
        self.assertIsNotNone(result)
        self.assertTrue(os.path.isabs(result))
        path, name = os.path.split(result)
        self.assertEqual(name, 'ls')
