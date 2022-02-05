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

from __future__ import absolute_import, print_function, unicode_literals

import json
import os
import sys


from build_swift.build_swift.constants import SWIFT_BUILD_ROOT


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


def log_time_path():
    return os.path.join(SWIFT_BUILD_ROOT, '.build_script_log')


def clear_log_time():
    f = open(log_time_path(), "w")
    f.close()


def log_time(event, command, duration=0):
    f = open(log_time_path(), "a")

    log_event = {
        "event": event,
        "command": command,
        "duration": "%.2f" % float(duration),
    }

    f.write("{}\n".format(json.dumps(log_event)))
    f.close()
