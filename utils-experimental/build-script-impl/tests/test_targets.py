# tests/test_targets.py -----------------------------------------*- python -*-
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

from build_script import targets


class TargetsTestCase(unittest.TestCase):

    def test_targets(self):

        self.assertEqual(targets.split('linux-x86_64'),
                         ('linux', 'x86_64'))

        self.assertEqual(targets.split('linux-x86_64-bar'),
                         ('linux', 'x86_64-bar'))

        self.assertEqual(targets.split('linux'),
                         ('linux', None))

        self.assertTrue(targets.is_osx('macosx-x86_64'))
        self.assertFalse(targets.is_osx('linux-armv7'))
        self.assertFalse(targets.is_osx('iphoneos-arm64'))

        self.assertTrue(targets.is_darwin_type('macosx-x86_64'))
        self.assertTrue(targets.is_darwin_type('iphonesimulator-i386'))
        self.assertTrue(targets.is_darwin_type('iphoneos-armv7s'))
        self.assertTrue(targets.is_darwin_type('appletvsimulator-x86_64'))
        self.assertTrue(targets.is_darwin_type('appletvos-arm64'))
        self.assertTrue(targets.is_darwin_type('watchsimulator-i386'))
        self.assertTrue(targets.is_darwin_type('watchos-armv7k'))
        self.assertFalse(targets.is_darwin_type('freebsd-x86_64'))
