#!/usr/bin/env python3

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

import os
import shutil
import subprocess
import sys
import tempfile
import unittest


MODULE_DIR = os.path.abspath(os.path.dirname(__file__))
UTILS_DIR = os.path.abspath(os.path.join(MODULE_DIR, os.pardir))


if __name__ == "__main__":
    # Add the swift/utils directory to the Python path.
    sys.path.append(UTILS_DIR)

    # Create temp directory and export it for the test suite.
    temp_dir = tempfile.mkdtemp()
    os.environ["UPDATE_CHECKOUT_TEST_SUITE_ARENA"] = temp_dir

    # Configure a deterministic local git identity and disable commit signing
    # for all clones.
    #
    # update-checkout's own clones (under `source_root`) do not set these up, so a
    # test that commits into such a clone needs this to happen first; otherwise the
    # commit depends on an ambient global identity, which is absent on a pristine
    # CI worker, and aborts.
    os.environ["GIT_CONFIG_SYSTEM"] = os.devnull
    os.environ["GIT_CONFIG_GLOBAL"] = os.path.join(temp_dir, ".gitconfig")
    subprocess.run(
        ["git", "config", "--global", "user.name", "swift_test"], check=True
    )
    subprocess.run(
        ["git", "config", "--global", "user.email", "no-reply@swift.org"], check=True
    )
    subprocess.run(
        ["git", "config", "--global", "commit.gpgsign", "false"], check=True
    )

    # Discover all tests for the module.
    module_tests = unittest.defaultTestLoader.discover(MODULE_DIR)

    # Create and run test suite.
    suite = unittest.TestSuite()
    suite.addTests(module_tests)

    runner = unittest.TextTestRunner(verbosity=2)
    try:
        result = runner.run(suite)
    finally:
        # Ensure the temp directory is removed
        shutil.rmtree(temp_dir, ignore_errors=True)

    sys.exit(not result.wasSuccessful())
