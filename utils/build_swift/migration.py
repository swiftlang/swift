# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Temporary module with functionaly used to migrate away from build-script-impl.
"""

from __future__ import absolute_import, unicode_literals

import itertools

from swift_build_support.swift_build_support.targets import \
    StdlibDeploymentTarget


try:
    # Python 2
    from itertools import imap
except ImportError:
    imap = map


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

    def __init__(self, sdk):
        self.sdk = sdk

        super(UnknownSDKError, self).__init__(
            'Unknown SDK: {}'.format(self.sdk))


def migrate_swift_sdks(args):
    """Migrate usages of the now deprecated `--swift-sdks` option to the new
    `--stdlib-deployment-targets` option, converting Swift SDKs to the
    corresponding targets.

    This function is a stop-gap to replacing all instances of `--swift-sdks`.
    """

    def _flatten(iterable):
        return itertools.chain.from_iterable(iterable)

    def _swift_sdk_to_stdlib_targets(sdk):
        targets = _SDK_TARGETS.get(sdk, None)
        if targets is None:
            raise UnknownSDKError(sdk)

        return targets

    def _migrate_swift_sdks_arg(arg):
        if not arg.startswith('--swift-sdks'):
            return arg

        sdks = arg.split('=')[1]
        sdk_list = [] if sdks == '' else sdks.split(';')

        targets = _flatten(imap(_swift_sdk_to_stdlib_targets, sdk_list))
        target_names = [target.name for target in targets]

        return '--stdlib-deployment-targets={}'.format(' '.join(target_names))

    return list(imap(_migrate_swift_sdks_arg, args))
