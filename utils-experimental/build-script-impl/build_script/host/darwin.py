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
from . import shell


class Darwin(Base):
    def __init__(self, **kwargs):
        super(Darwin, self).__init__(**kwargs)
        self.xcrun_sdk = 'macosx'
        self.xcrun_toolchain = None
        self.darwin_install_prefix = None

    def is_darwin(self):
        return True

    def system_memory_in_bytes(self):
        return shell.query(['sysctl', 'hw.memsize']).rstrip().split(' ')[1]

    def _xcrun_find(self, tool):
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
            return shell.query(command, stderr=subprocess.PIPE).rstrip()

        except subprocess.CalledProcessError:
            return None

    def sdk_path(sdk):
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
        return self._xcrun_find('clang')

    def find_clang_cxx(self):
        return self._xcrun_find('clang++')

    def find_cmake(self):
        return self._xcrun_find('cmake')

    @property
    def darwin_toolchain_prefix(self):
        if self.darwin_install_prefix is None:
            return None
        return os.path.dirname(self.darwin_install_prefix)


class MacOSX(Darwin):
    pass
