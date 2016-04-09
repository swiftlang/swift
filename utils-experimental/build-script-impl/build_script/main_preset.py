# build_script/main_preset.py -----------------------------------*- python -*-
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
"""
Preset mode entry point
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import sys
import os.path
import argparse

from . import env
from . import _shell
from .utils import printf_with_argv0
from .presets import (
    get_all_preset_names,
    get_preset_options,
)


def main(argv):
    '''
    Main entry point for the preset mode.
    '''

    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="""Builds Swift using a preset.""")
    parser.add_argument(
        "--preset-file",
        help="load presets from the specified file",
        metavar="PATH",
        action="append",
        dest="preset_file_names",
        default=[])
    parser.add_argument(
        "--preset",
        help="use the specified option preset",
        metavar="NAME")
    parser.add_argument(
        "--show-presets",
        help="list all presets and exit",
        action="store_true")
    parser.add_argument(
        "--distcc",
        help="use distcc",
        action="store_true")

    parser.add_argument(
        "-n", "--dry-run",
        help="Print the commands that would be executed, but do not execute "
             "them",
        default=False,
        action="store_true")

    parser.add_argument(
        "preset_substitutions_raw",
        help="'name=value' pairs that are substituted in the preset",
        nargs="*",
        metavar="SUBSTITUTION")
    args = parser.parse_args()

    if len(args.preset_file_names) == 0:
        args.preset_file_names = [
            os.path.join(env.HOME, ".swift-build-presets"),
            os.path.join(
               env.SWIFT_SOURCE_ROOT, "swift", "utils", "build-presets.ini")
        ]

    if args.show_presets:
        for name in sorted(get_all_preset_names(args.preset_file_names),
                           key=str.lower):
            print(name)
        return 0

    if not args.preset:
        printf_with_argv0("Missing --preset option")
        return 1

    args.preset_substitutions = {}

    for arg in args.preset_substitutions_raw:
        name, value = arg.split("=", 1)
        args.preset_substitutions[name] = value

    preset_args = get_preset_options(
        args.preset_substitutions, args.preset_file_names, args.preset)

    build_script_args = [sys.argv[0]]
    if args.dry_run:
        build_script_args += ['-n']

    build_script_args += preset_args
    if args.distcc:
        build_script_args += ["--distcc"]

    print(
        "using preset '" + args.preset + "'")

    _shell.execv(build_script_args, echo=True)
