# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from .utils import TestCase
from .. import migration
from .. import shell


def _get_sdk_targets(sdk_names):
    targets = []
    for sdk_name in sdk_names:
        targets += migration._SDK_TARGETS[sdk_name]

    return targets


def _get_sdk_target_names(sdk_names):
    return [target.name for target in _get_sdk_targets(sdk_names)]


# -----------------------------------------------------------------------------

class TestMigrationMeta(type):
    """Metaclass used to dynamically generate test methods.
    """

    def __new__(cls, name, bases, attrs):
        # Generate tests for migrating each Swift SDK
        for sdk_name in migration._SDK_TARGETS.keys():
            test_name = 'test_migrate_swift_sdk_' + sdk_name
            attrs[test_name] = cls.generate_migrate_swift_sdks_test(sdk_name)

        return super(TestMigrationMeta, cls).__new__(
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


class TestMigration(TestCase):

    __metaclass__ = TestMigrationMeta

    def test_parse_args(self):
        pass

    def test_check_impl_args_calls_build_script_impl(self):
        sh = shell.NullExecutor()

        migration.check_impl_args([], command_executor=sh)
        self.assertListEqual(sh.history(), [
            [migration.BUILD_SCRIPT_IMPL_PATH, '--check-args-only=1'],
        ])

    def test_check_impl_args_known_arg(self):
        migration.check_impl_args(['--reconfigure'])

    def test_check_impl_args_unknown_arg(self):
        with self.assertRaises(ValueError):
            migration.check_impl_args(['--unknown-arg'])

    def test_migrate_multiple_swift_sdk_flags(self):
        args = [
            '--swift-sdks=OSX',
            '--swift-sdks=OSX;IOS;IOS_SIMULATOR'
        ]

        args = migration.migrate_swift_sdks(args)
        target_names = _get_sdk_target_names(['OSX', 'IOS', 'IOS_SIMULATOR'])

        self.assertListEqual(args, [
            '--stdlib-deployment-targets={}'.format(' '.join(target_names))
        ])
