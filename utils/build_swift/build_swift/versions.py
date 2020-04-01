# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Version parsing classes.
"""


from __future__ import absolute_import, unicode_literals

import functools

import six


__all__ = [
    'InvalidVersionError',
    'Version',
]


# -----------------------------------------------------------------------------
# Version Parsing

class _ComponentType(object):
    """Poor-man's enum representing all valid version character groups.
    """

    def __init__(self, name):
        self.name = name

    def __eq__(self, other):
        if not isinstance(other, _ComponentType):
            return NotImplemented

        return self.name == other.name

    def __ne__(self, other):
        return not self.__eq__(other)

    @classmethod
    def _register(cls, name):
        setattr(cls, name, cls(name))


_ComponentType._register('ALPHA_LOWER')
_ComponentType._register('ALPHA_UPPER')
_ComponentType._register('DOT')
_ComponentType._register('NUMERIC')
_ComponentType._register('OTHER')


def _get_component_type(component):
    """Classifies a component into one of the registered component types.
    """

    if len(component) <= 0:
        raise ValueError('Empty component')

    if component == '.':
        return _ComponentType.DOT

    if component.isdigit():
        return _ComponentType.NUMERIC

    if component.isalpha():
        if component.isupper():
            return _ComponentType.ALPHA_UPPER
        elif component.islower():
            return _ComponentType.ALPHA_LOWER
        else:
            raise ValueError('Unknown component type for {!r}'.format(
                component))

    return _ComponentType.OTHER


def _try_cast(obj, cls):
    """Attempts to cast an object to a class, returning the resulting casted
    object or the original object if the cast raises a ValueError.
    """

    try:
        return cls(obj)
    except ValueError:
        return obj


def _split_version(version):
    """Splits a version string into a tuple of components using similar rules
    to distutils.version.LooseVersion. All version strings are valid, but the
    outcome will only split on boundries between:

        * lowercase alpha characters
        * uppercase alpha characters
        * numeric characters
        * the literal '.' (dot) character

    All other characters are grouped into an "other" category.

    Numeric components are converted into integers in the resulting tuple.

    An empty tuple is returned for the empty string.

    ```
    >>> _split_version('1000.2.108')
    (1000, 2, 28)

    >>> _split_version('10A23b')
    (10, 'A', 23, 'b')

    >>> _split_version('10.23-beta4')
    (10, 23, '-', 'beta', 4)

    >>> _split_version('FOObarBAZqux')
    ('FOO', 'bar', 'BAZ', 'qux')
    ```
    """

    if len(version) < 1:
        return tuple()

    components = []

    part = version[0]
    part_type = _get_component_type(part)

    for char in version[1:]:
        char_type = _get_component_type(char)

        if part_type == char_type:
            part += char
        else:
            components.append(part)
            part = char
            part_type = char_type

    # Add last part
    components.append(part)

    # Remove '.' groups and try casting components to ints
    components = (_try_cast(c, int) for c in components if c != '.')

    return tuple(components)


# -----------------------------------------------------------------------------
# Versions

class InvalidVersionError(Exception):
    """Error indicating an invalid version was encountered.
    """

    def __init__(self, version, msg=None):
        self.version = version

        if msg is None:
            msg = 'Invalid version: {}'.format(self.version)

        super(InvalidVersionError, self).__init__(msg)


@functools.total_ordering
class Version(object):
    """Similar to the standard distutils.versons.LooseVersion, but with a
    little more wiggle-room for alpha characters.
    """

    __slots__ = ('components', '_str')

    def __init__(self, version):
        version = six.text_type(version)

        # Save the version string since it's impossible to reconstruct it from
        # just the parsed components
        self._str = version

        # Parse version components
        self.components = _split_version(version)

    def __eq__(self, other):
        if not isinstance(other, Version):
            return NotImplemented

        return self.components == other.components

    # NOTE: Python 2 compatibility.
    def __ne__(self, other):
        return not self == other

    def __lt__(self, other):
        if not isinstance(other, Version):
            return NotImplemented

        return self.components < other.components

    def __hash__(self):
        return hash(self.components)

    def __str__(self):
        return self._str

    def __repr__(self):
        return '{}({!r})'.format(type(self).__name__, self._str)
