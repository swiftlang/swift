# ===-- utils.py ---------------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https:#swift.org/LICENSE.txt for license information
# See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===#

import sys


def fatal_error(message, stream=sys.stderr):
    """Writes a message to the given stream and exits. By default this
    function outputs to stderr.
    """

    stream.write('[{}] ERROR: {}\n'.format(sys.argv[0], message))
    stream.flush()
    sys.exit(1)


def exit_rejecting_arguments(message, parser=None):
    print(message, file=sys.stderr)
    if parser:
        parser.print_usage(sys.stderr)
    sys.exit(2)  # 2 is the same as `argparse` error exit code.
