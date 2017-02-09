# swift_build_support/tar.py - Call tar from Python -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from __future__ import absolute_import

import platform

from . import shell


def tar(source, destination):
    """
    Create a gzip archive of the file at 'source' at the given
    'destination' path.
    """
    # We do not use `tarfile` here because:
    #  - We wish to support LZMA2 compression while also supporting Python 2.7.
    #  - We wish to explicitly set the owner and group of the archive.
    args = ['tar', '-c', '-z', '-f', destination]

    if platform.system() != 'Darwin':
        args += ['--owner=0', '--group=0']

    # Discard stderr output such as 'tar: Failed to open ...'. We'll detect
    # these cases using the exit code, which should cause 'shell.call' to
    # raise.
    shell.call(args + [source], stderr=shell.DEVNULL)
