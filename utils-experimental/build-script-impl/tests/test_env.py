# tests/test_env.py ---------------------------------------------*- python -*-
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
import os

from build_script import env


class EnvTestCase(unittest.TestCase):

    def test_env_exists(self):
        self.assertIsNotNone(env.HOME)
        self.assertIsNotNone(env.SWIFT_SOURCE_ROOT)
        self.assertIsNotNone(env.SWIFT_BUILD_ROOT)

    def test_env_var(self):
        # Note: This test may fail if this script is moved
        # from the swift source tree.
        # Assuming :
        # {SWIFT_SOURCE_ROOT}/swift/utils/build-script-impl/tests/test_env.py

        self.assertEqual(
            os.path.realpath(env.SWIFT_SOURCE_ROOT),
            os.path.realpath(os.path.join(os.path.dirname(__file__),
                                          '..', '..', '..', '..')))
        self.assertEqual(
            os.path.realpath(env.SWIFT_BUILD_ROOT),
            os.path.realpath(os.path.join(os.path.dirname(__file__),
                                          '..', '..', '..', '..', 'build')))
