#!/usr/bin/python
# -*- coding: utf-8 -*-

# ===--- test_utils.py ---------------------------------------------------===//
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https://swift.org/LICENSE.txt for license information
#  See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===//
"""
Homegrown unit testing helpers.

The issue is that the unittest.mock was added in Python 3.3 and we need to run
on Python 2.7.

`Stub` and `Mock` are very rudimentary and support only the limited subset of
common unit testing patterns that is used in this project.
"""

import logging
import sys
from StringIO import StringIO
from contextlib import contextmanager


@contextmanager
def captured_output():
    """Capture stdout and stderr and return their output as string buffers."""
    new_out, new_err = StringIO(), StringIO()
    old_out, old_err = sys.stdout, sys.stderr
    try:
        sys.stdout, sys.stderr = new_out, new_err
        yield sys.stdout, sys.stderr
    finally:
        sys.stdout, sys.stderr = old_out, old_err


class Stub:
    """Object literal stub for value objects."""

    def __init__(self, **attributes):
        """All named parameters will create properties on this object."""
        self.__dict__.update(attributes)


class Mock(object):
    """Minimal infrastructure for manually mocking calls to single method.

    Classes inheriting from Mock are expected to create their own mock of the
    tested method with appropriate signature, which appends the call arguments
    as tuple to the `calls` list and returns the canned response retrieved from
    the `respond` dictionary.
    """

    def __init__(self, responses=None):
        """Optionaly initialized with a list of expected calls. See expect."""
        self.calls = []
        self.expected = []
        self.respond = dict()
        responses = responses or []
        for call_args, response in responses:
            self.expect(call_args, response)

    def expect(self, call_args, response):
        """Expect invocation of tested method with given arguments.

        Stores the canned reponse in the `respond` dictionary.
        """
        call_args = tuple(call_args)
        self.expected.append(call_args)
        self.respond[call_args] = response

    def assert_called_with(self, expected_args):
        """Verify that the tested method was called with provided arguments."""
        expected_args = tuple(expected_args)
        assert expected_args in self.calls, (
            'Expected: {0} in Called: {1}'.format(expected_args, self.calls))

    def assert_called_all_expected(self):
        """Verify that all expeced invocations of tested method were called."""
        assert self.calls == self.expected, (
            '\nExpected: {0}, \n  Called: {1}'.format(
                self.expected, self.calls))


class MockLoggingHandler(logging.Handler):
    """Mock logging handler to check for expected logs."""

    def __init__(self, *args, **kwargs):
        """Prepare the logger for recording messages for each log level."""
        self.reset()
        super(MockLoggingHandler, self).__init__(*args, **kwargs)

    def emit(self, record):
        """Store the message in list for the given log level."""
        self.messages[record.levelname.lower()].append(record.getMessage())

    def reset(self):
        """Clear all log messages."""
        self.messages = {
            'debug': [], 'info': [], 'warning': [], 'error': [], 'critical': []
        }
