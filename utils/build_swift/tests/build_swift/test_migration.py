# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import platform
import unittest

from build_swift import argparse
from build_swift import migration
from build_swift.constants import BUILD_SCRIPT_IMPL_PATH

import six


# -----------------------------------------------------------------------------
# Helpers

def _get_sdk_targets(sdk_names):
    targets = []
    for sdk_name in sdk_names:
        targets += migration._SDK_TARGETS[sdk_name]

    return targets


def _get_sdk_target_names(sdk_names):
    return [target.name for target in _get_sdk_targets(sdk_names)]


# -----------------------------------------------------------------------------
# Mirgrate Swift SDKs

class TestMigrateSwiftSDKsMeta(type):
    """Metaclass used to dynamically generate test methods.
    """

    def __new__(cls, name, bases, attrs):
        # Generate tests for migrating each Swift SDK
        for sdk_name in migration._SDK_TARGETS.keys():
            test_name = 'test_migrate_swift_sdk_{}'.format(sdk_name)
            attrs[test_name] = cls.generate_migrate_swift_sdks_test(sdk_name)

        return super(TestMigrateSwiftSDKsMeta, cls).__new__(
            cls, name, bases, attrs)

    @classmethod
    def generate_migrate_swift_sdks_test(cls, sdk_name):
        def test(self):
            args = ['--swift-sdks={}'.format(sdk_name)]
            args = migration.migrate_swift_sdks(args)

            target_names = _get_sdk_target_names([sdk_name])
            self.assertListEqual(args, [
                '--stdlib-deployment-targets={}'.format(' '.join(target_names))
            ])

        return test


@six.add_metaclass(TestMigrateSwiftSDKsMeta)
class TestMigrateSwiftSDKs(unittest.TestCase):

    def test_empty_swift_sdks(self):
        args = migration.migrate_swift_sdks(['--swift-sdks='])
        self.assertListEqual(args, ['--stdlib-deployment-targets='])

    def test_multiple_swift_sdk_flags(self):
        sdks = ['OSX', 'IOS', 'IOS_SIMULATOR']

        args = [
            '--swift-sdks=',
            '--swift-sdks={}'.format(';'.join(sdks))
        ]

        args = migration.migrate_swift_sdks(args)
        target_names = _get_sdk_target_names(sdks)

        self.assertListEqual(args, [
            '--stdlib-deployment-targets=',
            '--stdlib-deployment-targets={}'.format(' '.join(target_names))
        ])


# -----------------------------------------------------------------------------

class TestMigrateParseArgs(unittest.TestCase):

    def test_report_unknown_args(self):
        parser = argparse.ArgumentParser()
        parser.add_argument('-R', '--release', action='store_true')
        parser.add_argument('-T', '--validation-test', action='store_true')
        parser.add_argument('--darwin-xcrun-toolchain')

        args = migration.parse_args(parser, [
            '-RT',
            '--unknown', 'true',
            '--darwin-xcrun-toolchain=foo',
            '--',
            '--darwin-xcrun-toolchain=bar',
            '--other',
        ])

        expected = argparse.Namespace(
            release=True,
            validation_test=True,
            darwin_xcrun_toolchain='bar',
            build_script_impl_args=['--unknown', 'true', '--other'])

        self.assertEqual(args, expected)

    def test_no_unknown_args(self):
        parser = argparse.ArgumentParser()
        parser.add_argument('-R', '--release', action='store_true')
        parser.add_argument('-T', '--validation-test', action='store_true')
        parser.add_argument('--darwin-xcrun-toolchain')

        args = migration.parse_args(
            parser, ['-RT', '--darwin-xcrun-toolchain=bar'])

        expected = argparse.Namespace(
            release=True,
            validation_test=True,
            darwin_xcrun_toolchain='bar',
            build_script_impl_args=[])

        self.assertEqual(args, expected)

    def test_forward_impl_args(self):
        parser = argparse.ArgumentParser()
        parser.add_argument('--skip-test-swift',
                            dest='impl_skip_test_swift',
                            action='store_true')
        parser.add_argument('--install-swift',
                            dest='impl_install_swift',
                            action='store_true')

        args = migration.parse_args(
            parser, ['--skip-test-swift', '--install-swift'])

        expected = argparse.Namespace(
            build_script_impl_args=['--skip-test-swift', '--install-swift'])

        self.assertEqual(args, expected)


class TestMigrationCheckImplArgs(unittest.TestCase):

    def test_check_impl_args(self):
        if platform.system() == 'Windows':
            self.skipTest("build-script-impl cannot run in Windows")
            return

        self.assertIsNone(migration.check_impl_args(
            BUILD_SCRIPT_IMPL_PATH, ['--reconfigure']))

        with self.assertRaises(ValueError) as cm:
            migration.check_impl_args(
                BUILD_SCRIPT_IMPL_PATH, ['foo'])

        self.assertIn('foo', str(cm.exception))

        with self.assertRaises(ValueError) as cm:
            migration.check_impl_args(
                BUILD_SCRIPT_IMPL_PATH, ['--reconfigure', '--foo=true'])

        self.assertIn('foo', str(cm.exception))
