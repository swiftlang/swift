# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import functools
import os
import platform
import sys
import unittest

from build_swift import cache_utils
from build_swift.versions import Version

import six
from six import StringIO


__all__ = [
    'quiet_output',
    'redirect_stderr',
    'redirect_stdout',
    'requires_attr',
    'requires_module',
    'requires_platform',
    'requires_python',
]


# -----------------------------------------------------------------------------
# Constants

_PYTHON_VERSION = Version(platform.python_version())


# -----------------------------------------------------------------------------
# Helpers

def _can_import(fullname):
    try:
        __import__(fullname)
        return True
    except ImportError:
        return False


# -----------------------------------------------------------------------------

class quiet_output(object):
    """Context manager and decorator used to quiet both sys.stderr and
    sys.stdout by redirecting them to os.devnull.
    """

    __slots__ = ('_devnull', '_old_stderr', '_old_stdout')

    def __enter__(self):
        self._devnull = open(os.devnull, 'w')
        self._old_stderr = sys.stderr
        self._old_stdout = sys.stdout

        sys.stderr = self._devnull
        sys.stdout = self._devnull

    def __exit__(self, exc_type, exc_value, traceback):
        sys.stderr = self._old_stderr
        sys.stdout = self._old_stdout

        self._devnull.close()

    def __call__(self, func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            with self:
                return func(*args, **kwargs)

        return wrapper


class redirect_stderr(object):
    """Context manager used to substitute sys.stderr with a different file-like
    object.
    """

    __slots__ = ('_stream', '_old_stderr')

    def __init__(self, stream=None):
        self._stream = stream or StringIO()

    def __enter__(self):
        self._old_stderr, sys.stderr = sys.stderr, self._stream
        return self._stream

    def __exit__(self, exc_type, exc_value, traceback):
        sys.stderr = self._old_stderr


class redirect_stdout():
    """Context manager used to substitute sys.stdout with a different file-like
    object.
    """

    __slots__ = ('_stream', '_old_stdout')

    def __init__(self, stream=None):
        self._stream = stream or StringIO()

    def __enter__(self):
        self._old_stdout, sys.stderr = sys.stderr, self._stream
        return self._stream

    def __exit__(self, exc_type, exc_value, traceback):
        sys.stderr = self._old_stdout


@cache_utils.cache
def requires_attr(obj, attr):
    """Decorator used to skip tests if an object does not have the required
    attribute.
    """

    try:
        getattr(obj, attr)
        return lambda func: func
    except AttributeError:
        return unittest.skip('Required attribute "{}" not found on {}'.format(
            attr, obj))


@cache_utils.cache
def requires_module(fullname):
    """Decorator used to skip tests if a module is not imported.
    """

    if _can_import(fullname):
        return lambda func: func

    return unittest.skip('Unable to import "{}"'.format(fullname))


@cache_utils.cache
def requires_platform(name):
    """Decorator used to skip tests if not running on the given platform.
    """

    if name == platform.system():
        return lambda func: func

    return unittest.skip(
        'Required platform "{}" does not match system'.format(name))


@cache_utils.cache
def requires_python(version):
    """Decorator used to skip tests if the running Python version is not
    greater or equal to the required version.
    """

    if isinstance(version, six.string_types):
        version = Version(version)

    if _PYTHON_VERSION >= version:
        return lambda func: func

    return unittest.skip(
        'Requires Python version {} or greater'.format(version))
