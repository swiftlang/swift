# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from .utils import TestCase
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
