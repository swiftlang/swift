#!/usr/bin/env python3

# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Utility script used to run the black code formatter over all the Python scripts in the
project sources.
"""


import argparse
import os
import subprocess
import sys
from pathlib import Path


_SWIFT_PATH = Path(__file__).resolve().parents[1]

_KNOWN_SCRIPT_PATHS = [
    _SWIFT_PATH / "benchmark/scripts/Benchmark_Driver",
    _SWIFT_PATH / "benchmark/scripts/Benchmark_DTrace.in",
    _SWIFT_PATH / "benchmark/scripts/Benchmark_GuardMalloc.in",
    _SWIFT_PATH / "benchmark/scripts/Benchmark_QuickCheck.in",
    _SWIFT_PATH / "benchmark/scripts/Benchmark_RuntimeLeaksRunner.in",
    _SWIFT_PATH / "benchmark/scripts/run_smoke_bench",
    _SWIFT_PATH / "docs/scripts/ns-html2rst",
    _SWIFT_PATH / "test/Driver/Inputs/fake-toolchain/ld",
    _SWIFT_PATH / "utils/80+-check",
    _SWIFT_PATH / "utils/backtrace-check",
    _SWIFT_PATH / "utils/build-parser-lib",
    _SWIFT_PATH / "utils/build-script",
    _SWIFT_PATH / "utils/check-incremental",
    _SWIFT_PATH / "utils/coverage/coverage-build-db",
    _SWIFT_PATH / "utils/coverage/coverage-generate-data",
    _SWIFT_PATH / "utils/coverage/coverage-query-db",
    _SWIFT_PATH / "utils/coverage/coverage-touch-tests",
    _SWIFT_PATH / "utils/dev-scripts/blockifyasm",
    _SWIFT_PATH / "utils/dev-scripts/split-cmdline",
    _SWIFT_PATH / "utils/gyb",
    _SWIFT_PATH / "utils/line-directive",
    _SWIFT_PATH / "utils/PathSanitizingFileCheck",
    _SWIFT_PATH / "utils/recursive-lipo",
    _SWIFT_PATH / "utils/round-trip-syntax-test",
    _SWIFT_PATH / "utils/rth",
    _SWIFT_PATH / "utils/run-test",
    _SWIFT_PATH / "utils/scale-test",
    _SWIFT_PATH / "utils/submit-benchmark-results",
    _SWIFT_PATH / "utils/swift_build_support/tests/mock-distcc",
    _SWIFT_PATH / "utils/symbolicate-linux-fatal",
    _SWIFT_PATH / "utils/update-checkout",
    _SWIFT_PATH / "utils/viewcfg",
]


_INSTALL_BLACK_MESSAGE = """\
The black Python package is required for formatting, but it was not found on
your system.

You can install it using:

    python3 -m pip install black

For more help, see https://black.readthedocs.io.
"""


def _get_python_sources():
    """Returns a list of path objects for all known Python sources in the Swift
    project.
    """

    return list(_SWIFT_PATH.rglob("*.py")) + _KNOWN_SCRIPT_PATHS


def _is_package_installed(name):
    """Runs the pip command to check if a package is installed.
    """

    command = [
        sys.executable,
        "-m",
        "pip",
        "show",
        "--quiet",
        name,
    ]

    with open(os.devnull, "w") as devnull:
        status = subprocess.call(command, stderr=devnull)

    return not status


def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "paths",
        type=Path,
        metavar="PATH",
        nargs="*",
        help="Source path to format.",
    )

    parser.add_argument(
        "--check",
        action="store_true",
        help="Don't format the file, just retun the status.",
    )

    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Emit messages to stderr about files that were not changed.",
    )

    return parser.parse_args()


def main():
    args = parse_args()

    if not _is_package_installed("black"):
        print(_INSTALL_BLACK_MESSAGE)
        return 1

    command = [
        sys.executable,
        "-m",
        "black",
        "--target-version",
        "py27",
    ]

    if args.check:
        command.append("--check")
    if args.verbose:
        command.append("--verbose")

    requested_paths = [path.resolve() for path in args.paths]

    # Narrow down the set of paths to format to only those paths which are either
    # included in the set of requested paths or are subpaths of the requested paths.
    format_paths = {
        known_path
        for path in requested_paths
        for known_path in _get_python_sources()
        if path == known_path or path in known_path.parents
    }

    # Add requested paths that exists, but aren't included in the format set.
    for path in requested_paths:
        if path not in format_paths and path.exists():
            format_paths.add(path)

    command += sorted([str(path) for path in format_paths])

    return subprocess.call(command)


if __name__ == "__main__":
    sys.exit(main())
