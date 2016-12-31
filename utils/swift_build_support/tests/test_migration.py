# test_migration.py - Tests for swift_build_support.migration -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import argparse
import os
import unittest

from swift_build_support import migration


class MigrateImplArgsTestCase(unittest.TestCase):
    def test_report_unknown_args(self):
        parser = argparse.ArgumentParser()
        parser.add_argument('-R', '--release', action='store_true')
        parser.add_argument('-T', '--validation-test', action='store_true')
        parser.add_argument('--darwin-xcrun-toolchain')

        args = migration.parse_args(
            parser,
            ['-RT', '--unknown', 'true', '--darwin-xcrun-toolchain=foo', '--',
             '--darwin-xcrun-toolchain=bar', '--other'])

        self.assertEqual(
            args,
            argparse.Namespace(
                release=True,
                validation_test=True,
                darwin_xcrun_toolchain='bar',
                build_script_impl_args=['--unknown', 'true', '--other']))

    def test_no_unknown_args(self):
        parser = argparse.ArgumentParser()
        parser.add_argument('-R', '--release', action='store_true')
        parser.add_argument('-T', '--validation-test', action='store_true')
        parser.add_argument('--darwin-xcrun-toolchain')

        args = migration.parse_args(
            parser,
            ['-RT', '--darwin-xcrun-toolchain=bar'])

        self.assertEqual(
            args,
            argparse.Namespace(
                release=True,
                validation_test=True,
                darwin_xcrun_toolchain='bar',
                build_script_impl_args=[]))

    def test_check_impl_args(self):
        # Assuming file locations:
        #   utils/swift_build_support/tests/test_migration.py
        #   utils/build-script-impl
        build_script_impl = os.path.join(
            os.path.dirname(__file__), '..', '..', 'build-script-impl')

        self.assertIsNone(migration.check_impl_args(build_script_impl,
                                                    ['--reconfigure']))

        with self.assertRaises(ValueError) as cm:
            migration.check_impl_args(build_script_impl, ['foo'])
        self.assertIn('foo', str(cm.exception))

        with self.assertRaises(ValueError) as cm:
            migration.check_impl_args(build_script_impl, ['--reconfigure',
                                                          '--foo=true'])
        self.assertIn('foo', str(cm.exception))


if __name__ == '__main__':
    unittest.main()
