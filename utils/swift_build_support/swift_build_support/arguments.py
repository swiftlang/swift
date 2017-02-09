# swift_build_support/arguments.py ------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
"""
argparse supplements
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import argparse
import os
import re
import shlex

__all__ = [
    "action",
    "type",
]


class _Registry(object):
    pass


def _register(registry, name, value):
    setattr(registry, name, value)


# Types ----------------------------------------------------------------------
type = _Registry()


def type_bool(string):
    """
    A strict parser for bools

    unlike Python's `bool()`, where `bool('False')` is `True`
    This function can be passed as `type=` argument to argparse to parse values
    passed to command line arguments.
    """
    if string in ['0', 'false', 'False']:
        return False
    if string in ['1', 'true', 'True']:
        return True
    raise argparse.ArgumentTypeError("%r is not a boolean value" % string)

_register(type, 'bool', type_bool)


def type_shell_split(string):
    """
    Parse and split shell arguments string into a list of shell arguments.

    Recognize `,` as a separator as well as white spaces.
    string: -BAR="foo bar" -BAZ='foo,bar',-QUX 42
    into
    ['-BAR=foo bar', '-BAZ=foo,bar', "-QUX", "42"]
    """
    lex = shlex.shlex(string, posix=True)
    lex.whitespace_split = True
    lex.whitespace += ','
    return list(lex)

_register(type, 'shell_split', type_shell_split)


class CompilerVersion(object):
    """A typed representation of a compiler version."""

    def __init__(self, string_representation, components):
        self.string_representation = string_representation
        self.components = components

    def __str__(self):
        return self.string_representation


def type_clang_compiler_version(string):
    """
    Parse version string and split into a tuple of strings
    (major, minor, patch)

    Supports "MAJOR.MINOR.PATCH" and "MAJOR.MINOR.PATCH.PATCH" formats.
    """
    m = re.match(r'^([0-9]+)\.([0-9]+)\.([0-9]+)(\.([0-9]+))?$', string)
    if m is not None:
        return CompilerVersion(
            string_representation=string,
            components=m.group(1, 2, 3, 5))
    raise argparse.ArgumentTypeError(
        "%r is an invalid version value, "
        "must be 'MAJOR.MINOR.PATCH' or "
        "'MAJOR.MINOR.PATCH.PATCH'" % string)

_register(type, 'clang_compiler_version', type_clang_compiler_version)


def type_swift_compiler_version(string):
    """
    Parse version string and split into a tuple of strings
    (major, minor, patch)

    Supports "MAJOR.MINOR" and "MAJOR.MINOR.PATCH" formats.
    """
    m = re.match(r'^([0-9]+)\.([0-9]+)(\.([0-9]+))?$', string)
    if m is not None:
        return CompilerVersion(
            string_representation=string,
            components=m.group(1, 2, 4))
    raise argparse.ArgumentTypeError(
        "%r is an invalid version value, "
        "must be 'MAJOR.MINOR' or "
        "'MAJOR.MINOR.PATCH'" % string)

_register(type, 'swift_compiler_version', type_swift_compiler_version)


def type_executable(string):
    """
    Check the string is executable path string.

    Convert it to absolute path.
    """
    if os.path.isfile(string) and os.access(string, os.X_OK):
        return os.path.abspath(string)
    raise argparse.ArgumentTypeError(
        "%r is not executable" % string)

_register(type, 'executable', type_executable)

# Actions --------------------------------------------------------------------
action = _Registry()


class _UnavailableAction(argparse.Action):
    def __init__(self,
                 option_strings,
                 dest=argparse.SUPPRESS,
                 default=argparse.SUPPRESS,
                 nargs='?',
                 help=None):
        super(_UnavailableAction, self).__init__(
            option_strings=option_strings,
            dest=dest,
            default=default,
            nargs=nargs,
            help=help)

    def __call__(self, parser, namespace, values, option_string=None):
        if option_string is not None:
            arg = option_string
        else:
            arg = str(values)
        parser.error('unknown argument: %s' % arg)

_register(action, 'unavailable', _UnavailableAction)


class _ConcatAction(argparse.Action):

    def __call__(self, parser, namespace, values, option_string=None):
        old_val = getattr(namespace, self.dest)
        if old_val is None:
            val = values
        else:
            val = old_val + values
        setattr(namespace, self.dest, val)

_register(action, 'concat', _ConcatAction)


class _OptionalBoolAction(argparse.Action):
    def __init__(self,
                 option_strings,
                 dest,
                 default=False,
                 metavar="BOOL",
                 help=None):
        super(_OptionalBoolAction, self).__init__(
            option_strings=option_strings,
            dest=dest,
            default=default,
            metavar=metavar,
            nargs="?",
            type=type.bool,
            help=help,
            const=True)

    def __call__(self, parser, namespace, values, option_string=None):
        setattr(namespace, self.dest, values)

_register(action, 'optional_bool', _OptionalBoolAction)
