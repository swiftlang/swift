# test_targets.py - Unit tests for swift_build_support.targets -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import unittest

from swift_build_support.targets import StdlibDeploymentTarget


class HostTargetTestCase(unittest.TestCase):
    def test_is_not_none_on_this_platform(self):
        self.assertIsNotNone(StdlibDeploymentTarget.host_target())


class PlatformTargetsTestCase(unittest.TestCase):
    def test_platform_contains(self):
        """
        Checks that Platform.contains(target_name)
        matches all of its targets' names and rejects non-matching names.
        """
        # Pick a few platforms with lots of targets
        for platform in [StdlibDeploymentTarget.Linux,
                         StdlibDeploymentTarget.iOS,
                         StdlibDeploymentTarget.iOSSimulator]:
            for target in platform.targets:
                self.assertTrue(platform.contains(target.name))
            self.assertFalse(platform.contains("fakeCPU-MSDOS"))
            self.assertFalse(platform.contains("singleTransistor-fakeOS"))

if __name__ == '__main__':
    unittest.main()
