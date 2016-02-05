#!/usr/bin/env python

# utils/profdata_merge/main.py
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# This module is used to prevent profile data filling up available disk space
# by listening for profile data and merging them into a universal profdata
# file while tests are executing.
# This file invokes the runner after parsing arguments.

from __future__ import print_function
import sys
import argparse
import tempfile
import logging

import runner

SERVER_ADDRESS = ('localhost', 12400)
TESTS_FINISHED_SENTINEL = "PROFDATA_MERGE_WORKER_TESTS_FINISHED_SENTINEL"

if __name__ == "__main__":
    if sys.platform != "darwin":
        sys.exit("Error: The profile data merge worker requires OS X.")

    parser = argparse.ArgumentParser()
    parser.add_argument("-l", "--log-file",
                        help="The file to write logs in debug mode.")

    subparsers = parser.add_subparsers()

    start = subparsers.add_parser("start")
    start.add_argument("-d", "--debug",
                       help="Run in foreground and report status.",
                       action="store_true")
    start.add_argument("-o", "--output-dir",
                       help=("The directory to write the PID file" +
                             "and final profdata file."),
                       default=tempfile.gettempdir())
    start.add_argument("--no-remove",
                       action="store_true",
                       help="Don't remove profraw files after merging them.")
    start.set_defaults(func=runner.start_server)

    stop = subparsers.add_parser("stop")
    stop.set_defaults(func=runner.stop_server)

    args = parser.parse_args()

    log_args = {'level': logging.DEBUG}
    if args.log_file:
        log_args['filename'] = args.log_file

    logging.basicConfig(**log_args)

    args.func(args)
