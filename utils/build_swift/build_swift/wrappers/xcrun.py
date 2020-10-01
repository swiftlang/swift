# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Wrapper module around the 'xcrun' command-line utility.
"""


from __future__ import absolute_import, unicode_literals

import functools
import re
import shlex

import six

from .. import shell
from ..versions import Version


__all__ = [
    'XcrunWrapper',
]


# -----------------------------------------------------------------------------
# Constants

_VERSION_PATTERN = re.compile(r'^xcrun version (?P<version>[0-9.]+)\.$')


# -----------------------------------------------------------------------------
# Helpers

def _catch_return_none(exceptions):
    """Decorator used to catch exceptions and return None.
    """

    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            try:
                return func(*args, **kwargs)
            except exceptions:
                return None

        return wrapper
    return decorator


def _prepend_sdk_and_toolchain(func):
    """Method decorator used to prepend the sdk and toolchain arguments to the
    final command passed to xcrun.
    """

    @functools.wraps(func)
    def wrapper(self, args, sdk=None, toolchain=None, **kwargs):
        if isinstance(args, six.string_types):
            args = shlex.split(args)
        if toolchain:
            args = ['--toolchain', toolchain] + args
        if sdk:
            args = ['--sdk', sdk] + args

        return func(self, args, **kwargs)
    return wrapper


# -----------------------------------------------------------------------------

class XcrunWrapper(shell.ExecutableWrapper):
    """Wrapper class around the 'xcrun' command-line utility.
    """

    EXECUTABLE = shell.which('xcrun') or 'xcrun'

    @_prepend_sdk_and_toolchain
    def Popen(self, args, **kwargs):
        return super(XcrunWrapper, self).Popen(args, **kwargs)

    @_prepend_sdk_and_toolchain
    def call(self, args, **kwargs):
        return super(XcrunWrapper, self).call(args, **kwargs)

    @_prepend_sdk_and_toolchain
    def check_call(self, args, **kwargs):
        return super(XcrunWrapper, self).check_call(args, **kwargs)

    @_prepend_sdk_and_toolchain
    def check_output(self, args, **kwargs):
        return super(XcrunWrapper, self).check_output(args, **kwargs)

    # -------------------------------------------------------------------------

    @property
    @_catch_return_none(shell.CalledProcessError)
    def version(self):
        """Returns the xcrun version.
        """

        output = self.check_output('--version')
        matches = _VERSION_PATTERN.match(output.rstrip())
        if matches:
            return Version(matches.group('version'))

        return None

    # -------------------------------------------------------------------------
    # Subcommands

    @_catch_return_none(shell.CalledProcessError)
    def find(self, tool, sdk=None, toolchain=None):
        """Finds and returns the path to tool using xcrun. Returns None if the
        tool cannot be found.
        """

        return self.check_output(
            ['--find', tool],
            sdk=sdk,
            toolchain=toolchain,
            stderr=shell.DEVNULL).rstrip()

    def kill_cache(self):
        """Kills the xcrun cache. Returns the status code from the command.
        """

        return self.call('--kill-cache')

    @_catch_return_none(shell.CalledProcessError)
    def sdk_path(self, sdk=None, toolchain=None):
        """Returns the SDK path. Returns None if the SDK cannot be found.
        """

        return self.check_output(
            '--show-sdk-path',
            sdk=sdk,
            toolchain=toolchain,
            stderr=shell.DEVNULL).rstrip()

    @_catch_return_none(shell.CalledProcessError)
    def sdk_version(self, sdk=None, toolchain=None):
        """Returns the SDK version. Returns None if the SDK cannot be found.
        """

        output = self.check_output(
            '--show-sdk-version',
            sdk=sdk,
            toolchain=toolchain,
            stderr=shell.DEVNULL)

        return Version(output.rstrip())

    @_catch_return_none(shell.CalledProcessError)
    def sdk_build_version(self, sdk=None, toolchain=None):
        """Returns the SDK build version. Returns None if the SDK cannot be
        found.
        """

        output = self.check_output(
            '--show-sdk-build-version',
            sdk=sdk,
            toolchain=toolchain,
            stderr=shell.DEVNULL)

        return Version(output.rstrip())

    @_catch_return_none(shell.CalledProcessError)
    def sdk_platform_path(self, sdk=None, toolchain=None):
        """Returns the SDK platform path. Returns None if the SDK cannot be
        found.
        """

        return self.check_output(
            '--show-sdk-platform-path',
            sdk=sdk,
            toolchain=toolchain,
            stderr=shell.DEVNULL).rstrip()

    @_catch_return_none(shell.CalledProcessError)
    def sdk_platform_version(self, sdk=None, toolchain=None):
        """Returns the SDK platform version. Returns None if the SDK cannot be
        found.
        """

        output = self.check_output(
            '--show-sdk-platform-version',
            sdk=sdk,
            toolchain=toolchain,
            stderr=shell.DEVNULL)

        return Version(output.rstrip())
