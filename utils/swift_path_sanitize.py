# utils/swift_path_sanitize.py -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

# Shared path-sanitization logic used by the PathSanitizingFileCheck and
# PathSanitizingDiff front-ends. Both tools apply the same replacements to
# their input; they only differ in how they compare the sanitized text
# (FileCheck directives vs. diff against a reference file).

import os
import platform
import re


# LLVM Lit performs realpath with the config path, so all paths are relative
# to the real path. Paths that come from CMake (like cmake_binary_dir and
# swift_src_root), might not do real path. Use realpath to normalize. Because
# this normalizes Windows paths to use backslashes, we have to replace them
# back to forward slashes.
def normalize_if_path(s):
    # Check dirname for cases like a file named `%t.out.txt`
    # There won't be a `%t` path, but we still want to match this path substring.
    if not os.path.exists(s) and not os.path.exists(os.path.dirname(s)):
        return s
    if platform.system() == "Windows":
        return os.path.abspath(s).replace("\\", "/")
    else:
        return os.path.realpath(s)


def add_shared_arguments(parser):
    """Register the arguments that control path sanitization.

    These are common to every front-end that sanitizes its input before
    handing it off to a comparison tool.
    """
    parser.add_argument(
        "--sanitize",
        help="replace the given string with another string",
        metavar="REPLACEMENT=SOURCE",
        action="append",
        dest="sanitize_strings",
        default=[],
    )

    parser.add_argument(
        "--enable-windows-compatibility",
        help="Enable Windows path compatibility, which checks against both "
        "forward slashes and backward slashes.",
        action="store_true",
    )

    parser.add_argument(
        "--enable-yaml-compatibility",
        help="Enable YAML path compatibility. Since YAML double escapes "
        "backward slashes, we need to check for them escaped. Only "
        "available if Windows compatibility is enabled.",
        action="store_true",
    )

    parser.add_argument(
        "--ignore-runtime-warnings",
        help="Ignore warnings from the Swift runtime",
        action="store_true",
    )


def sanitize(text, args):
    """Apply the path replacements described by ``args`` to ``text``.

    ``args`` is the parsed namespace produced by a parser that had
    ``add_shared_arguments`` applied to it.
    """
    if args.enable_windows_compatibility:
        if args.enable_yaml_compatibility:
            slashes_re = r"(/|\\\\|\\\\\\\\)"
        else:
            slashes_re = r"(/|\\\\)"
    else:
        slashes_re = r"/"

    for s in sorted(args.sanitize_strings, key=len, reverse=True):
        replacement, pattern = s.split("=", 1)
        # Since we want to use pattern as a regex in some platforms, we need
        # to escape it first, and then replace the escaped slash
        # literal (r'\\/') for our platform-dependent slash regex.
        text = re.sub(
            re.sub(r"/", slashes_re, re.escape(normalize_if_path(pattern))),
            replacement,
            text,
        )

    # Because we force the backtracer on in the tests, we can get runtime
    # warnings about privileged programs.  Suppress those, and also the
    # warning it might emit if backtracing isn't supported on the test platform.
    # Additionally, suppress warnings about unknown backtracer options, since
    # we might want to add new ones to the lit tests and we should ignore
    # messages from the system copy of the runtime in that case.
    if args.ignore_runtime_warnings:
        text = re.sub(
            r"^swift runtime: (backtrace-on-crash is not " r"supported|unknown) .*\n",
            "",
            text,
            flags=re.M,
        )

    return text


# MARK: PathSanitizingDiff utils (shared with the lit plugin)

# Name of the PathSanitizingDiff option (added automatically by the
# %PathSanitizingDiff lit substitution) that carries the test's temporary
# location (%t). PathSanitizingDiff writes its sanitized input into an output
# derived from it so the update plugin can find it.
TEMP_DIR_OPTION = "--temp-dir"

# Suffix of the output file into which PathSanitizingDiff writes its sanitized
# input on a mismatch. See output_path for how the full name is formed.
UPDATE_ACTUAL_SUFFIX = ".actual"


def reference_path(args):
    """Return the reference (expected) file from a PathSanitizingDiff arg list.

    The reference file is passed as the trailing positional argument, so return
    the last argument that is not an option. Both PathSanitizingDiff and the
    update plugin rely on this so they agree on the output location.
    """
    for arg in reversed(args):
        if not arg.startswith("-"):
            return arg
    return None


def temp_dir_from_args(args):
    """Return the value of the last --temp-dir option in a PathSanitizingDiff
    arg list, or None if absent. Handles both "--temp-dir X" and
    "--temp-dir=X" spellings.
    """
    result = None
    expect_value = False
    for arg in args:
        if expect_value:
            result = arg
            expect_value = False
        elif arg == TEMP_DIR_OPTION:
            expect_value = True
        elif arg.startswith(TEMP_DIR_OPTION + "="):
            result = arg[len(TEMP_DIR_OPTION) + 1 :]
    return result


def output_path(temp_dir, reference):
    """Return the path PathSanitizingDiff writes its sanitized input to.

    The output always lives in the per-test temporary namespace so that
    parallel tests cannot collide, regardless of where the reference file lives:
      - if temp_dir is a directory, it is placed inside it as
        "<temp_dir>/<basename(reference)>.actual";
      - otherwise temp_dir is treated as a path prefix and the output is placed in
      "<temp_dir>.<basename(reference)>.actual".
    """
    name = os.path.basename(reference) + UPDATE_ACTUAL_SUFFIX
    if os.path.isdir(temp_dir):
        return os.path.join(temp_dir, name)
    return temp_dir + "." + name
