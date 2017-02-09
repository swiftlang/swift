# swift_build_support/migration.py - Migrating build-script -*- python -*-
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
# utils/build-script takes arguments for its argument parser, as well as
# arguments that are meant to be passed directly to utils/build-script-impl.
# In order to gradually migrate away from build-script-impl, this module
# provides tools to handle parsing of these args.
#
# ----------------------------------------------------------------------------

import subprocess


def parse_args(parser, argv):
    """
    Parse given argument list with given argparse.ArgumentParser.

    Return a processed arguments object. Any unknown arguments are stored in
    `build_script_impl_args` attribute as a list.
    Ignores '--' to be compatible with old style argument list.

        build-script -RT -- --reconfigure
    """
    args, unknown_args = parser.parse_known_args(
        list(arg for arg in argv if arg != '--'))
    args.build_script_impl_args = unknown_args
    return args


def check_impl_args(build_script_impl, argv):
    """
    Check whether given argv are all known arguments for `build-script-impl`.

    Raise ValueError with message if any invalid argument is found.
    Return nothing if success.
    """
    pipe = subprocess.Popen(
        [build_script_impl, '--check-args-only=1'] + argv,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)

    (_, err) = pipe.communicate()

    if pipe.returncode != 0:
        msg = str(err.splitlines()[0].decode())
        raise ValueError(msg)
