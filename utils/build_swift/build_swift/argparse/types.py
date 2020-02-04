# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Argument types useful for enforcing data-integrity and form when parsing
arguments.
"""


from __future__ import absolute_import, unicode_literals

import os.path
import re
import shlex

import six

from . import ArgumentTypeError
from ..versions import Version


__all__ = [
    'BoolType',
    'PathType',
    'RegexType',
    'ClangVersionType',
    'SwiftVersionType',
    'ShellSplitType',
]


# -----------------------------------------------------------------------------

def _repr(cls, args):
    """Helper function for implementing __repr__ methods on *Type classes.
    """

    _args = []
    for key, value in six.iteritems(args):
        _args.append('{}={}'.format(key, repr(value)))

    return '{}({})'.format(type(cls).__name__, ', '.join(_args))


class BoolType(object):
    """Argument type used to validate an input string as a bool-like type.
    Callers are able to override valid true and false values.
    """

    TRUE_VALUES = [True, 1, 'TRUE', 'True', 'true', '1']
    FALSE_VALUES = [False, 0, 'FALSE', 'False', 'false', '0']

    def __init__(self, true_values=None, false_values=None):
        true_values = true_values or BoolType.TRUE_VALUES
        false_values = false_values or BoolType.FALSE_VALUES

        self._true_values = set(true_values)
        self._false_values = set(false_values)

    def __call__(self, value):
        if value in self._true_values:
            return True
        elif value in self._false_values:
            return False
        else:
            raise ArgumentTypeError('{} is not a boolean value'.format(value))

    def __repr__(self):
        return _repr(self, {
            'true_values': self._true_values,
            'false_values': self._false_values,
        })


class PathType(object):
    """PathType denotes a valid path-like object. When called paths will be
    fully expanded with the option to assert the file or directory referenced
    by the path exists.
    """

    def __init__(self, assert_exists=False, assert_executable=False):
        self._assert_exists = assert_exists
        self._assert_executable = assert_executable

    def __call__(self, path):
        path = os.path.expanduser(path)
        path = os.path.abspath(path)

        if self._assert_exists and not os.path.exists(path):
            raise ArgumentTypeError('{} does not exist'.format(path))

        if self._assert_executable and not PathType._is_executable(path):
            raise ArgumentTypeError('{} is not an executable'.format(path))

        return path

    def __repr__(self):
        return _repr(self, {
            'assert_exists': self._assert_exists,
            'assert_executable': self._assert_executable,
        })

    @staticmethod
    def _is_executable(path):
        return os.path.isfile(path) and os.access(path, os.X_OK)


class RegexType(object):
    """Argument type used to validate an input string against a regular
    expression.
    """

    def __init__(self, regex, error_message=None):
        self._regex = regex
        self._error_message = error_message or 'Invalid value'

    def __call__(self, value):
        matches = re.match(self._regex, value)
        if matches is None:
            raise ArgumentTypeError(self._error_message, value)

        return matches

    def __repr__(self):
        return _repr(self, {
            'regex': self._regex,
            'error_message': self._error_message,
        })


class ClangVersionType(RegexType):
    """Argument type used to validate Clang version strings.
    """

    ERROR_MESSAGE = ('Invalid version value, must be '
                     '"MAJOR.MINOR.PATCH" or "MAJOR.MINOR.PATCH.PATCH"')

    VERSION_REGEX = r'^(\d+)\.(\d+)\.(\d+)(\.(\d+))?$'

    def __init__(self):
        super(ClangVersionType, self).__init__(
            ClangVersionType.VERSION_REGEX,
            ClangVersionType.ERROR_MESSAGE)

    def __call__(self, value):
        super(ClangVersionType, self).__call__(value)
        return Version(value)


class SwiftVersionType(RegexType):
    """Argument type used to validate Swift version strings.
    """

    ERROR_MESSAGE = ('Invalid version value, must be "MAJOR.MINOR" '
                     ', "MAJOR.MINOR.PATCH" or "MAJOR.MINOR.PATCH.PATCH"')
    VERSION_REGEX = r'^(\d+)\.(\d+)(\.(\d+))?(\.(\d+))?$'

    def __init__(self):
        super(SwiftVersionType, self).__init__(
            SwiftVersionType.VERSION_REGEX,
            SwiftVersionType.ERROR_MESSAGE)

    def __call__(self, value):
        super(SwiftVersionType, self).__call__(value)
        return Version(value)


class ShellSplitType(object):
    """Parse and split shell arguments into a list of strings. Recognizes `,`
    as a separator as well as white spaces.

    For example it converts the following:

    '-BAR="foo bar" -BAZ="foo,bar",-QUX 42'

    into

    ['-BAR=foo bar', '-BAZ=foo,bar', '-QUX', '42']
    """

    def __call__(self, value):
        lex = shlex.shlex(value, posix=True)
        lex.whitespace_split = True
        lex.whitespace += ','
        return list(lex)

    def __repr__(self):
        return _repr(self, {})
