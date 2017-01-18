# swift_build_support/xcrun.py - Invoke xcrun from Python -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
#
# Python wrappers for invoking `xcrun` on the command-line.
#
# ----------------------------------------------------------------------------

from __future__ import absolute_import

from . import cache_util
from . import shell


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

    # `xcrun --find` prints to stderr when it fails to find the
    # given tool. We swallow that output with a pipe.
    out = shell.capture(
        command,
        stderr=shell.DEVNULL, dry_run=False, echo=False, optional=True)
    if out is None:
        return None
    return out.rstrip()


@cache_util.cached
def sdk_path(sdk):
    """
    Return the path string for given SDK, according to `xcrun --show-sdk-path`.

    If `xcrun --show-sdk-path` cannot find the SDK, return None.
    """
    command = ['xcrun', '--sdk', sdk, '--show-sdk-path']
    out = shell.capture(command, dry_run=False, echo=False, optional=True)
    if out is None:
        return None
    return out.rstrip()
