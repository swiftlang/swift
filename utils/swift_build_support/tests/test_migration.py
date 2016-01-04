# test_migration.py - Tests for swift_build_support.migration -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import unittest

from swift_build_support.migration import migrate_impl_args


class MigrateImplArgsTestCase(unittest.TestCase):
    def test_args_moved_before_separator(self):
        # Tests that '-RT --foo=bar -- --foo=baz --flim' is parsed as
        # '-RT --foo=bar --foo=baz -- --flim'
        args = migrate_impl_args(
            ['-RT', '--darwin-xcrun-toolchain=foo', '--',
             '--darwin-xcrun-toolchain=bar', '--other'],
            ['--darwin-xcrun-toolchain'])

        self.assertEqual(
            args,
            ['-RT', '--darwin-xcrun-toolchain=foo',
             '--darwin-xcrun-toolchain=bar', '--', '--other'])

if __name__ == '__main__':
    unittest.main()
