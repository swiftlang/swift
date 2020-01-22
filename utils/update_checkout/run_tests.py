#!/usr/bin/env python

# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Small script used to easily run the update_checkout module unit tests.
"""


from __future__ import absolute_import, unicode_literals

import os
import shutil
import sys
import tempfile
import unittest


MODULE_DIR = os.path.abspath(os.path.dirname(__file__))
UTILS_DIR = os.path.abspath(os.path.join(MODULE_DIR, os.pardir))


if __name__ == '__main__':
    # Add the swift/utils directory to the Python path.
    sys.path.append(UTILS_DIR)

    # Create temp directory and export it for the test suite.
    temp_dir = tempfile.mkdtemp()
    os.environ['UPDATECHECKOUT_TEST_WORKSPACE_DIR'] = temp_dir

    # Discover all tests for the module.
    module_tests = unittest.defaultTestLoader.discover(MODULE_DIR)

    # Create and run test suite.
    suite = unittest.TestSuite()
    suite.addTests(module_tests)

    runner = unittest.TextTestRunner()
    try:
        result = runner.run(suite)
    finally:
        # Ensure the temp directory is removed
        shutil.rmtree(temp_dir, ignore_errors=True)

    sys.exit(not result.wasSuccessful())
