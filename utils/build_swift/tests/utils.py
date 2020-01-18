# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import os
import sys
import unittest
from contextlib import contextmanager
from io import StringIO


__all__ = [
    'add_metaclass',
    'redirect_stderr',
    'redirect_stdout',
    'TestCase',

    'BUILD_SCRIPT_IMPL_PATH',
    'BUILD_SWIFT_PATH',
    'TESTS_PATH',
    'UTILS_PATH',
]


# -----------------------------------------------------------------------------
# Constants

TESTS_PATH = os.path.abspath(os.path.dirname(__file__))
BUILD_SWIFT_PATH = os.path.abspath(os.path.join(TESTS_PATH, os.pardir))
UTILS_PATH = os.path.abspath(os.path.join(BUILD_SWIFT_PATH, os.pardir))

BUILD_SCRIPT_IMPL_PATH = os.path.join(UTILS_PATH, 'build-script-impl')


# -----------------------------------------------------------------------------

def add_metaclass(metacls):
    def wrapper(cls):
        body = vars(cls).copy()

        body.pop('__dict__', None)
        body.pop('__weakref__', None)

        return metacls(cls.__name__, cls.__bases__, body)

    return wrapper


@contextmanager
def redirect_stderr(stream=None):
    stream = stream or StringIO()
    old_stderr, sys.stderr = sys.stderr, stream
    try:
        yield stream
    finally:
        sys.stderr = old_stderr


@contextmanager
def redirect_stdout(stream=None):
    stream = stream or StringIO()
    old_stdout, sys.stdout = sys.stdout, stream
    try:
        yield stream
    finally:
        sys.stdout = old_stdout


# -----------------------------------------------------------------------------

class TestCase(unittest.TestCase):

    @contextmanager
    def quietOutput(self):
        with open(os.devnull, 'w') as devnull:
            with redirect_stderr(devnull), redirect_stdout(devnull):
                yield
