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


def _get_default_source_root():
    result = ""

    # Are we in a Swift checkout? Start from this file and check its parent
    # directories.
    #
    # $SWIFT_SOURCE_ROOT/swift/utils/SwiftBuildSupport.py
    (swift_path, parent_dirname) = os.path.split(os.path.dirname(__file__))
    if parent_dirname != "utils":
        return result
    if not os.path.exists(os.path.join(swift_path, 'CMakeLists.txt')):
        return result
    result = os.path.dirname(swift_path)

    # Are we in an LLVM checkout? Start from the Swift checkout and check /its/
    # parent directories.
    #
    # $SWIFT_SOURCE_ROOT/llvm/tools/swift/utils/SwiftBuildSupport.py
    (llvm_path, parent_dirname) = os.path.split(result)
    if parent_dirname != "tools":
        return result
    if not os.path.exists(os.path.join(llvm_path, 'CMakeLists.txt')):
        return result
    result = os.path.dirname(llvm_path)

    return result

# Set SWIFT_SOURCE_ROOT in your environment to control where the sources
# are found.
SWIFT_SOURCE_ROOT = os.environ.get(
    "SWIFT_SOURCE_ROOT", _get_default_source_root())

# Set SWIFT_BUILD_ROOT to a directory that will contain a subdirectory
# for each build configuration
SWIFT_BUILD_ROOT = os.environ.get(
    "SWIFT_BUILD_ROOT", os.path.join(SWIFT_SOURCE_ROOT, "build"))


def print_with_argv0(message):
    print(sys.argv[0] + ": " + message)


def bad_usage(message):
    print_with_argv0(message)
    print("Run '" + pipes.quote(sys.argv[0]) + " --help' for more information.")
    sys.exit(1)

def quote_shell_command(args):
    return " ".join([ pipes.quote(a) for a in args ])


def check_call(args, print_command=False, verbose=False):
    if print_command:
        print(os.getcwd() + "$ " + quote_shell_command(args))
    try:
        return subprocess.check_call(args)
    except subprocess.CalledProcessError as e:
        if verbose:
            print_with_argv0(e.strerror)
        else:
            print_with_argv0(
                "command terminated with a non-zero exit status " +
                str(e.returncode) + ", aborting")
        sys.stdout.flush()
        sys.exit(1)
    except OSError as e:
        print_with_argv0("could not execute '" + quote_shell_command(args) +
            "': " + e.strerror)
        sys.stdout.flush()
        sys.exit(1)


def check_output(args, print_command=False, verbose=False):
    if print_command:
        print(os.getcwd() + "$ " + quote_shell_command(args))
    try:
        return subprocess.check_output(args)
    except subprocess.CalledProcessError as e:
        if verbose:
            print_with_argv0(e.strerror)
        else:
            print_with_argv0(
                "command terminated with a non-zero exit status " +
                str(e.returncode) + ", aborting")
        sys.stdout.flush()
        sys.exit(1)
    except OSError as e:
        print_with_argv0("could not execute '" + quote_shell_command(args) +
            "': " + e.strerror)
        sys.stdout.flush()
        sys.exit(1)


def _load_preset_files_impl(preset_file_names, substitutions={}):
    config = ConfigParser.SafeConfigParser(substitutions, allow_no_value=True)
    if config.read(preset_file_names) == []:
        print_with_argv0(
            "preset file not found (tried " + str(preset_file_names) + ")")
        sys.exit(1)
    return config


_PRESET_PREFIX = "preset: "

def _get_preset_options_impl(config, substitutions, preset_name):
    section_name = _PRESET_PREFIX + preset_name
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
                    _get_preset_options_impl(config, substitutions, mixin)
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
    config = _load_preset_files_impl(preset_file_names, substitutions)

    (build_script_opts, build_script_impl_opts) = \
        _get_preset_options_impl(config, substitutions, preset_name)
    if build_script_opts is None:
        print_with_argv0("preset '" + preset_name + "' not found")
        sys.exit(1)

    return build_script_opts + [ "--" ] + build_script_impl_opts


def get_all_preset_names(preset_file_names):
    config = _load_preset_files_impl(preset_file_names)
    return [ name[len(_PRESET_PREFIX):] for name in config.sections()
             if name.startswith(_PRESET_PREFIX) ]


# A context manager for changing the current working directory.
#
#     with WorkingDirectory('/tmp'):
#         ... do work in /tmp...
class WorkingDirectory(object):
    def __init__(self, new_cwd):
        self.new_cwd = new_cwd

    def __enter__(self):
        self.old_cwd = os.getcwd()
        os.chdir(self.new_cwd)

    def __exit__(self, type, value, traceback):
        os.chdir(self.old_cwd)

