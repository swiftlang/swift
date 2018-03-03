# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

from .utils import TestCase
from .. import shell
from ..xcrun import Xcrun


class TestXcrun(TestCase):

    def test_build_command(self):
        xcrun = Xcrun()
        self.assertEqual(xcrun._build_command([]), ['xcrun'])

        xcrun = Xcrun(sdk='default', toolchain='default')
        self.assertEqual(xcrun._build_command(['--find', 'clang']), [
            'xcrun',
            '--sdk', 'default',
            '--toolchain', 'default',
            '--find', 'clang',
        ])

    def test_override_sdk(self):
        sh = shell.NullExecutor()
        xcrun = Xcrun(sh, sdk='iphoneos')
        self.assertEqual(xcrun._sdk, 'iphoneos')

        xcrun.popen([], sdk='tvos')
        self.assertEqual(sh.history()[0], ['xcrun', '--sdk', 'tvos'])

    def test_override_toolchain(self):
        sh = shell.NullExecutor()
        xcrun = Xcrun(sh, toolchain='default')
        self.assertEqual(xcrun._toolchain, 'default')

        xcrun.popen([], toolchain='test')
        self.assertEqual(sh.history()[0], ['xcrun', '--toolchain', 'test'])
