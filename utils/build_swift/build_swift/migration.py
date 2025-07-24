# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Temporary module with functionality used to migrate away from build-script-impl.
"""


from argparse import ArgumentParser
import itertools
import subprocess

from swift_build_support.swift_build_support.targets import \
    StdlibDeploymentTarget


__all__ = [
    'UnknownSDKError',

    'check_impl_args',
    'migrate_swift_sdks',
    'parse_args',
]


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
        targets = StdlibDeploymentTarget.get_migrated_targets_for_sdk(sdk)
        if targets is None:
            raise UnknownSDKError(sdk)

        return targets

    def _migrate_swift_sdks_arg(arg):
        if not arg.startswith('--swift-sdks'):
            return arg

        sdks = arg.split('=')[1]
        sdk_list = [] if sdks == '' else sdks.split(';')

        targets = _flatten(map(_swift_sdk_to_stdlib_targets, sdk_list))
        target_names = [target.name for target in targets]

        return '--stdlib-deployment-targets={}'.format(' '.join(target_names))

    return list(map(_migrate_swift_sdks_arg, args))


# -----------------------------------------------------------------------------

def _process_disambiguation_arguments(args, unknown_args):
    """These arguments are only listed in the driver arguments to stop argparse
    from auto expanding arguments like --install-swift to the known argument
    --install-swiftevolve. Remove them from args and add them to unknown_args
    again.
    """

    if hasattr(args, 'impl_skip_test_swift'):
        if args.impl_skip_test_swift:
            unknown_args.append('--skip-test-swift')
        del args.impl_skip_test_swift

    if hasattr(args, 'impl_install_swift'):
        if args.impl_install_swift:
            unknown_args.append('--install-swift')
        del args.impl_install_swift

    return args, unknown_args


def parse_args(parser: ArgumentParser, args, namespace=None):
    """Parses a given argument list with the given argparse.ArgumentParser.

    Return a processed arguments object. Any unknown arguments are stored in
    `build_script_impl_args` attribute as a list. Ignores '--' to be compatible
    with old style argument list.
    """

    args = [arg for arg in args if arg != '--']
    args, unknown_args = parser.parse_known_args(args, namespace)
    args, unknown_args = _process_disambiguation_arguments(args, unknown_args)

    args.build_script_impl_args = unknown_args

    return args


# -----------------------------------------------------------------------------

def check_impl_args(build_script_impl, args):
    """Check whether given argv are all known arguments for
    `build-script-impl`.

    Raises a ValueError if any invalid argument is found. Return nothing
    otherwise.
    """

    pipe = subprocess.Popen(
        [build_script_impl, '--check-args-only=1'] + args,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)

    _, err = pipe.communicate()

    if pipe.returncode != 0:
        raise ValueError(str(err.splitlines()[0].decode()))
