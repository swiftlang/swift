# swift_build_support/diagnostics.py - Diagnostic Utilities     -*- python -*-
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

import sys


def note(message):
    """
    note(message)

    Print a diagnostic notification to the standard output.
    """
    print(sys.argv[0] + ": note: " + message)
    sys.stdout.flush()


def fatal(message):
    """
    fatal(message)

    Raise a fatal error.
    """
    raise SystemExit(sys.argv[0] + ": fatal error: " + message)
