# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

import os
import sys
import unittest
from contextlib import contextmanager


__all__ = [
    'redirect_stderr',
    'redirect_stdout',
    'TestCase',
]


# -----------------------------------------------------------------------------

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

    @contextmanager
    def assertNotRaises(self, exception=BaseException):
        assert issubclass(exception, BaseException)

        try:
            yield
        except exception as e:
            message = '{} raised: {}'.format(exception.__name__, str(e))
            raise self.failureException(message)
