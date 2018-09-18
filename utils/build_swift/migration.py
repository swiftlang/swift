# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Temporary module with functionaly used to migrate away from build-script-impl.
"""

from __future__ import absolute_import, unicode_literals

from swift_build_support.swift_build_support.targets import \
    StdlibDeploymentTarget


__all__ = [
    'UnknownSDKError',
    'migrate_swift_sdks',
]


_SDK_TARGETS = {
    'OSX': StdlibDeploymentTarget.OSX.targets,
    'IOS': StdlibDeploymentTarget.iOS.targets,
    'IOS_SIMULATOR': StdlibDeploymentTarget.iOSSimulator.targets,
    'TVOS': StdlibDeploymentTarget.AppleTV.targets,
    'TVOS_SIMULATOR': StdlibDeploymentTarget.AppleTVSimulator.targets,
    'WATCHOS': StdlibDeploymentTarget.AppleWatch.targets,
    'WATCHOS_SIMULATOR': StdlibDeploymentTarget.AppleWatchSimulator.targets,
}


# -----------------------------------------------------------------------------

class UnknownSDKError(Exception):
    """Error indicating an unknown SDK was encountered when migrating to target
    triples.
    """

    pass


def _swift_sdks_to_stdlib_targets(swift_sdks):
    stdlib_targets = []
    for sdk in swift_sdks:
        sdk_targets = _SDK_TARGETS.get(sdk, None)
        if sdk_targets is None:
            raise UnknownSDKError(sdk)
        stdlib_targets += sdk_targets

    return stdlib_targets


def migrate_swift_sdks(args):
    """Migrate usages of the now deprecated `--swift-sdks` option to the new
    `--stdlib-deployment-targets` option, converting Swift SDKs to the
    corresponding targets. Since argument parsing is a last-wins scenario, only
    the last `--swift-sdks` option is migrated, all others are removed.

    This function is a stop-gap to replacing all instances of `--swift-sdks`.
    """

    swift_sdks_args = [arg for arg in args if arg.startswith('--swift-sdks')]

    if len(swift_sdks_args) < 1:
        return args

    # Only get the last --swift-sdks arg since last-wins
    swift_sdks_arg = swift_sdks_args[-1]

    sdk_list = swift_sdks_arg.split('=')[1].split(';')
    stdlib_targets = _swift_sdks_to_stdlib_targets(sdk_list)

    target_names = ' '.join([target.name for target in stdlib_targets])
    stdlib_targets_arg = '--stdlib-deployment-targets=' + target_names

    args = [arg for arg in args if not arg.startswith('--swift-sdks')]
    args.append(stdlib_targets_arg)

    return args
