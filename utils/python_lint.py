#!/usr/bin/env python

# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#


"""
Utility script used to run the flake8 linter over all the project Python
sources.
"""


from __future__ import absolute_import, print_function, unicode_literals

import os
import subprocess
import sys


__all__ = [
    'lint',
]


# -----------------------------------------------------------------------------
# Constants

_UTILS_DIR = os.path.abspath(os.path.dirname(__file__))
_PROJECT_DIR = os.path.dirname(_UTILS_DIR)

_REQUIRED_PACKAGES = [
    'flake8',
    'flake8-import-order',
]

_INSTALL_FLAKE8_MESSAGE = """
The flake8 and flake8-import-order Python packages are required for linting,
but these were not found on your system.

You can install these using:

    python -m pip install flake8
    python -m pip install flake8-import-order

For more help, see http://flake8.pycqa.org.
"""


# -----------------------------------------------------------------------------
# Helpers

def _is_package_installed(name):
    """Runs the pip command to check if a package is installed.
    """

    command = [
        sys.executable,
        '-m', 'pip',
        'show', '--quiet',
        name,
    ]

    with open(os.devnull, 'w') as devnull:
        status = subprocess.call(command, stderr=devnull)

    return not status


# -----------------------------------------------------------------------------

def lint(args, verbose=False):
    all_packages_installed = all([
        _is_package_installed(name)
        for name in _REQUIRED_PACKAGES
    ])

    if not all_packages_installed:
        if verbose:
            print(_INSTALL_FLAKE8_MESSAGE)

        return 1

    return subprocess.call(
        [sys.executable, '-m', 'flake8'] + args,
        cwd=_PROJECT_DIR,
        universal_newlines=True)


if __name__ == '__main__':
    sys.exit(lint(sys.argv[1:], verbose=True))
