# build_script/host/darwin.py -----------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
"""
Darwin based host class
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import os
import subprocess

from .base import Base
from .. import shell
from ..utils import printf

__all__ = [
    'MacOSX',
]


class Darwin(Base):
    def __init__(self, **kwargs):
        super(Darwin, self).__init__(**kwargs)
        self.xcrun_sdk = 'macosx'
        self.xcrun_toolchain = None

    def is_darwin(self):
        return True

    def system_memory_in_bytes(self):
        ret = shell.query(['sysctl', 'hw.memsize']).rstrip().split(' ')[1]
        return int(ret)

    def xcrun_find(self, tool):
        """\
        Return the path for the given tool, according to `xcrun --find`.
        If `xcrun --find` cannot find the tool, return None.
        """
        command = ['xcrun', '--find', tool]
        if self.xcrun_sdk is not None:
            command += ['--sdk', self.xcrun_sdk]
        if self.xcrun_toolchain is not None:
            command += ['--toolchain', self.xcrun_toolchain]

        try:
            # `xcrun --find` prints to stderr when it fails to find the
            # given tool. We swallow that output with a pipe.
            return shell.query(command,
                               stderr=subprocess.PIPE,
                               echo_=False).rstrip()

        except subprocess.CalledProcessError:
            return None

    def sdk_path(self, sdk):
        """\
        Return the SDK root path string for the given SDK name.
        """
        command = ['xcrun', '--sdk', sdk, '--show-sdk-path']
        try:
            # `xcrun --find` prints to stderr when it fails to find the
            # given tool. We swallow that output with a pipe.
            return shell.query(command, stderr=subprocess.PIPE).rstrip()

        except subprocess.CalledProcessError:
            return None

    def find_clang_cc(self):
        return self.xcrun_find('clang')

    def find_clang_cxx(self):
        return self.xcrun_find('clang++')

    def find_cmake(self):
        return self.xcrun_find('cmake')

    def toolchain_prefix(self, install_prefix):
        return os.path.dirname(install_prefix)

    def show_sdks(self):
        """
        Print the host machine's `xcodebuild` version, as well as version
        information for all available SDKs.
        """
        printf('{0}\n', shell.query(['xcodebuild', '-version'],
                                    echo_=False))
        printf('--- SDK versions ---')
        printf('{0}\n', shell.query(['xcodebuild', '-version', '-sdk'],
                                    echo_=False))


class MacOSX(Darwin):
    pass
