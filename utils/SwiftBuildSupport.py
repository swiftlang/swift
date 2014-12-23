#===--- SwiftBuildSupport.py - Utilities for Swift build scripts -----------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===------------------------------------------------------------------------===#

from __future__ import print_function

import ConfigParser
import os
import pipes
import subprocess
import sys


HOME = os.environ["HOME"]


# Set SWIFT_SOURCE_ROOT in your environment to control where the sources
# are found.
SWIFT_SOURCE_ROOT = os.environ.get(
    "SWIFT_SOURCE_ROOT", os.path.join(HOME, "src", "s"))


# Set SWIFT_BUILD_ROOT to a directory that will contain a subdirectory
# for each build configuration
SWIFT_BUILD_ROOT = os.environ.get(
    "SWIFT_BUILD_ROOT", os.path.join(HOME, "build", "swift"))


def print_with_argv0(message):
    print(sys.argv[0] + ": " + message)


def bad_usage(message):
    print_with_argv0(message)
    print("Run '" + pipes.quote(sys.argv[0]) + " --help' for more information.")
    sys.exit(1)


def quote_shell_command(args):
    return " ".join([ pipes.quote(a) for a in args ])


def check_call(args, verbose=False):
    try:
        return subprocess.check_call(args)
    except:
        if verbose:
            print_with_argv0("failed command: " + quote_shell_command(args))
        else:
            print_with_argv0("command terminated with a non-zero exit status, aborting build")
        sys.stdout.flush()
        sys.exit(1)


def check_output(args, verbose=False):
    try:
        return subprocess.check_output(args)
    except:
        if verbose:
            print_with_argv0("failed command: " + quote_shell_command(args))
        else:
            print_with_argv0("command terminated with a non-zero exit status, aborting build")
        sys.stdout.flush()
        sys.exit(1)


def get_preset_options_impl(config, substitutions, preset_name):
    section_name = "preset: " + preset_name
    if section_name not in config.sections():
        return (None, None)

    build_script_opts = []
    build_script_impl_opts = []
    dash_dash_seen = False
    for o, a in config.items(section_name):
        if o in substitutions:
            continue

        opt = None
        if o == "mixin-preset":
            # Split on newlines and filter out empty lines.
            mixins = filter(None, [m.strip() for m in a.splitlines()])
            for mixin in mixins:
                (base_build_script_opts, base_build_script_impl_opts) = \
                    get_preset_options_impl(config, substitutions, mixin)
                build_script_opts += base_build_script_opts
                build_script_impl_opts += base_build_script_impl_opts
        elif o == "dash-dash":
            dash_dash_seen = True
        elif a == "":
            opt = "--" + o
        else:
            opt = "--" + o + "=" + a

        if opt:
            if not dash_dash_seen:
                build_script_opts.append(opt)
            else:
                build_script_impl_opts.append(opt)
    return (build_script_opts, build_script_impl_opts)


def get_preset_options(substitutions, preset_file_names, preset_name):
    config = ConfigParser.SafeConfigParser(substitutions, allow_no_value=True)
    if config.read(preset_file_names) == []:
        print_with_argv0(
            "preset file not found (tried " + str(preset_file_names) + ")")
        sys.exit(1)

    (build_script_opts, build_script_impl_opts) = \
        get_preset_options_impl(config, substitutions, preset_name)
    if build_script_opts is None:
        print_with_argv0("preset '" + preset_name + "'not found")
        sys.exit(1)

    return build_script_opts + [ "--" ] + build_script_impl_opts


