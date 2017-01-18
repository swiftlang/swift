# main.py - Push executables and run them on an Android device -*- python -*-
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
# lit tests assume a single program can be invoked to execute Swift code and
# make expectations upon the output. This program is a wrapper that has the
# same API, but runs the Swift program on an Android device instead of on the
# host.
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import os
import sys

from adb.commands import execute_on_device


def _usage(program_name):
    return 'usage: {} [executable_path] [executable_arguments]'.format(
        program_name)


def _help(program_name):
    return '{}\n\n'.format(_usage(program_name)) + \
        'positional arguments:\n' + \
        '\texecutable_path\t\tThe path to a local executable that is to ' + \
        'be run on a connected Android device.\n' + \
        '\texecutable_arguments\tAdditional arguments that are to be ' + \
        'given to the executable when it is run on the device.\n'


def main(args=sys.argv):
    """
    The main entry point for adb_test_runner.

    Parse arguments and kick off the script. Return zero to indicate success,
    a non-zero integer otherwise.
    """
    # We don't use argparse, because we need to be able to pass
    # --arbitrary -params --like=this to the executable we're running
    # on device.
    program_name = os.path.basename(args.pop(0))

    if len(args) == 1 and args[0] in ['-h', '--help']:
        print(_help(program_name))
        return 0

    try:
        executable_path, executable_arguments = args[0], args[1:]
    except IndexError:
        print(_usage(program_name))
        print('{}: error: argument "executable_path" is required'.format(
            program_name))
        return 1

    return execute_on_device(executable_path, executable_arguments)
