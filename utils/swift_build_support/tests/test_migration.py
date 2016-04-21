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
import os

from swift_build_support import migration


class MigrateImplArgsTestCase(unittest.TestCase):
    def test_args_moved_before_separator(self):
        # Tests that '-RT --foo=bar -- --foo=baz --flim' is parsed as
        # '-RT --foo=bar --foo=baz -- --flim'
        args = migration.migrate_impl_args(
            ['-RT', '--darwin-xcrun-toolchain=foo', '--',
             '--darwin-xcrun-toolchain=bar', '--other'],
            ['--darwin-xcrun-toolchain'])

        self.assertEqual(
            args,
            ['-RT', '--darwin-xcrun-toolchain=foo',
             '--darwin-xcrun-toolchain=bar', '--', '--other'])

    def test_check_impl_args(self):
        # Assuming file locations:
        #   utils/swift_build_support/tests/test_migration.py
        #   utils/build-script-impl
        build_script_impl = os.path.join(
            os.path.dirname(__file__), '..', '..', 'build-script-impl')

        self.assertIsNone(migration.check_impl_args(build_script_impl,
                                                    ['--reconfigure']))

        # FIXME: self.assertRaises context manager is not py2.6 compatible.
        with self.assertRaises(ValueError) as cm:
            migration.check_impl_args(build_script_impl, ['foo'])
        self.assertIn('foo', str(cm.exception))

        with self.assertRaises(ValueError) as cm:
            migration.check_impl_args(build_script_impl, ['--reconfigure',
                                                          '--foo=true'])
        self.assertIn('foo', str(cm.exception))


if __name__ == '__main__':
    unittest.main()
