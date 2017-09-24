#!/usr/bin/env python
# python_lint.py - Runs flake8 linting over the repository ------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#

from __future__ import print_function

import os
import subprocess
import sys


def lint(arguments, verbose=True):
    flake8_result = subprocess.call(
        [sys.executable, "-c", "import flake8; import flake8_import_order"]
    )
    if flake8_result != 0:
        if verbose:
            print("""
The flake8 and flake8-import-order Python packages are required for linting,
but these were not found on your system.

You can install these using:

    python -m pip install flake8
    python -m pip install flake8-import-order

For more help, see http://flake8.pycqa.org.""")

        # We should be returning `flake8_result` from here.  However,
        # some Python files lint themselves using embedded doctests,
        # which causes CI smoke tests to fail because the Linux nodes
        # do not have these modules installed.

        return 0

    utils_directory = os.path.dirname(os.path.abspath(__file__))
    parent_directory = os.path.dirname(utils_directory)
    linting_result = subprocess.call(
        [sys.executable, "-m", "flake8"] + arguments,
        cwd=parent_directory,
        universal_newlines=True
    )
    return linting_result


if __name__ == '__main__':
    linting_result = lint(sys.argv[1:])
    sys.exit(linting_result)
