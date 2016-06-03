# swift_build_support/debug.py - Print information on the build -*- python -*-
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
# Convenient functions for printing out information on the build process.
#
# ----------------------------------------------------------------------------

from __future__ import absolute_import
from __future__ import print_function

import sys

from . import shell


def print_xcodebuild_versions(file=sys.stdout):
    """
    Print the host machine's `xcodebuild` version, as well as version
    information for all available SDKs.
    """
    version = shell.capture(
        ['xcodebuild', '-version'], dry_run=False, echo=False).rstrip()
    sdks = shell.capture(
        ['xcodebuild', '-version', '-sdk'], dry_run=False, echo=False).rstrip()
    fmt = """\
{version}

--- SDK versions ---
{sdks}

"""
    print(fmt.format(version=version, sdks=sdks), file=file)
    file.flush()
