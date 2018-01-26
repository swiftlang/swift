# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Xcrun wrapper module.
"""


from . import shell


__all__ = [
    'Xcrun',
]


class Xcrun(object):
    """Wrapper class around the Xcrun utility. Provides methods for each
    subcommand available.
    """

    def __init__(self, command_executor=None, sdk=None, toolchain=None):
        self._sh = command_executor or shell.CommandExecutor()
        self._sdk = sdk
        self._toolchain = toolchain

    def _build_command(self, args):
        command = ['xcrun']
        if self._sdk is not None:
            command += ['--sdk', self._sdk]
        if self._toolchain is not None:
            command += ['--toolchain', self._toolchain]

        return command + args

    def popen(self, args, **kwargs):
        return self._sh.popen(self._build_command(args), **kwargs)

    def call(self, args):
        return self._sh.call(self._build_command(args))

    def check_call(self, args):
        return self._sh.check_call(self._build_command(args))

    def check_output(self, args):
        return self._sh.check_output(self._build_command(args))

    # -------------------------------------------------------------------------

    def find(self, tool):
        return self.check_output(['-find', tool]).rstrip()

    def sdk_path(self):
        return self.check_output(['--show-sdk-path']).rstrip()

    def sdk_version(self):
        return self.check_output(['--show-sdk-version']).rstrip()

    def sdk_build_version(self):
        return self.check_output(['--show-sdk-build-version']).rstrip()

    def sdk_platform_path(self):
        return self.check_output(['--show-sdk-platform-path']).rstrip()

    def sdk_platform_version(self):
        return self.check_output(['--show-sdk-platform-version']).rstrip()

    def version(self):
        return self.check_output(['--version']).rstrip()
