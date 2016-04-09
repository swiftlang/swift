# tests/test_host.py --------------------------------------------*- python -*-
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

from build_script.host import host


class HostTestCase(unittest.TestCase):

    def test_is_xxx(self):

        result = [
            host.is_darwin(),
            host.is_linux(),
            host.is_freebsd(),
            host.is_cygwin()]

        true_count = 0
        for x in result:
            if x:
                true_count += 1

        self.assertEqual(true_count, 1)

    def test_find_xxx(self):

        if host.is_darwin():
            host.xcrun_toolchain = 'default'

        cc = host.find_clang_cc()
        cxx = host.find_clang_cxx()
        cmake = host.find_cmake()
        ninja = host.find_ninja()
        distcc = host.find_distcc()
        pump = host.find_distcc_pump()

        self.assertIsNotNone(cc)
        self.assertIsNotNone(cxx)
        self.assertIsNotNone(cmake)

        self.assertTrue(os.path.basename(cc).startswith('clang'))
        self.assertTrue(os.path.basename(cxx).startswith('clang++'))
        self.assertTrue(os.path.basename(cmake) == 'cmake')
        self.assertTrue(ninja is None or
                        os.path.basename(ninja) == 'ninja')
        self.assertTrue(distcc is None or
                        os.path.basename(ninja) == 'distcc')
        self.assertTrue(pump is None or
                        os.path.basename(pump) == 'pump' or
                        os.path.basename(pump) == 'distcc-pump')

    @unittest.skipUnless(host.is_darwin(),
                         'xcrun_find is available in darwin only')
    def test_xcrun_find(self):
        host.xcrun_toolchain = 'default'

        self.assertIsNotNone(host.xcrun_find('strip'))
        self.assertIsNotNone(host.xcrun_find('dsymutil'))

        lipo = host.xcrun_find('lipo')
        self.assertIsInstance(lipo, str)
        self.assertIsNotNone(lipo)
        self.assertTrue(os.path.isabs(lipo))

        self.assertIsNone(host.xcrun_find('invalid-tool-name-here'))

    @unittest.skipUnless(host.is_darwin(),
                         'sdk_path is available in darwin only')
    def test_sdk_path(self):
        host.xcrun_toolchain = 'default'

        self.assertIsNotNone(host.sdk_path('macosx'))
        self.assertIsNotNone(host.sdk_path('iphoneos'))
        self.assertIsNotNone(host.sdk_path('iphonesimulator'))
        self.assertIsNotNone(host.sdk_path('appletvos'))
        self.assertIsNotNone(host.sdk_path('appletvsimulator'))
        self.assertIsNotNone(host.sdk_path('watchos'))
        self.assertIsNotNone(host.sdk_path('watchsimulator'))

        sdkpath = host.sdk_path('macosx')
        self.assertIsInstance(sdkpath, str)
        self.assertFalse(sdkpath.endswith('\n'))
        self.assertTrue(os.path.isdir(sdkpath))
        self.assertTrue(os.path.isabs(sdkpath))

        self.assertIsNone(host.sdk_path('does-not-exists'))
