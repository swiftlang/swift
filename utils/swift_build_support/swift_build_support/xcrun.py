# swift_build_support/xcrun.py - Invoke xcrun from Python -*- python -*-
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
#
# Python wrappers for invoking `xcrun` on the command-line.
#
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import subprocess

from . import cache_util


@cache_util.cached
def find(tool, sdk=None, toolchain=None):
    """
    Return the path for the given tool, according to `xcrun --find`, using
    the given sdk and toolchain.

    If `xcrun --find` cannot find the tool, return None.
    """
    command = ['xcrun', '--find', tool]
    if sdk is not None:
        command += ['--sdk', sdk]
    if toolchain is not None:
        command += ['--toolchain', toolchain]

    try:
        # `xcrun --find` prints to stderr when it fails to find the
        # given tool. We swallow that output with a pipe.
        out = subprocess.check_output(command,
                                      stderr=subprocess.PIPE)
        return str(out.rstrip().decode())
    except subprocess.CalledProcessError:
        return None


@cache_util.cached
def sdk_path(sdk):
    """
    Return the path string for given SDK, according to `xcrun --show-sdk-path`.

    If `xcrun --show-sdk-path` cannot find the SDK, return None.
    """
    command = ['xcrun', '--sdk', sdk, '--show-sdk-path']
    try:
        out = subprocess.check_output(command)
        return str(out.rstrip().decode())
    except subprocess.CalledProcessError:
        return None
