# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

from .utils import TestCase, add_metaclass
from .. import migration


def _get_sdk_targets(sdk_names):
    targets = []
    for sdk_name in sdk_names:
        targets += migration._SDK_TARGETS[sdk_name]

    return targets


def _get_sdk_target_names(sdk_names):
    return [target.name for target in _get_sdk_targets(sdk_names)]


# -----------------------------------------------------------------------------

class TestMigrateSwiftSDKsMeta(type):
    """Metaclass used to dynamically generate test methods.
    """

    def __new__(cls, name, bases, attrs):
        # Generate tests for migrating each Swift SDK
        for sdk_name in migration._SDK_TARGETS.keys():
            test_name = 'test_migrate_swift_sdk_' + sdk_name
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


@add_metaclass(TestMigrateSwiftSDKsMeta)
class TestMigrateSwiftSDKs(TestCase):

    def test_multiple_swift_sdk_flags(self):
        args = [
            '--swift-sdks=OSX',
            '--swift-sdks=OSX;IOS;IOS_SIMULATOR'
        ]

        args = migration.migrate_swift_sdks(args)
        target_names = _get_sdk_target_names(['OSX', 'IOS', 'IOS_SIMULATOR'])

        self.assertListEqual(args, [
            '--stdlib-deployment-targets={}'.format(' '.join(target_names))
        ])
