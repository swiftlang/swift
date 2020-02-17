# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Class utility functions and decorators.
"""


from __future__ import absolute_import, unicode_literals


__all__ = [
    'generate_repr',
]


def generate_repr(*attrs):
    """Generates a standardized __repr__ implementation for the decorated class
    using the provided attributes and the class name.
    """

    def _repr(self):
        args = []
        for attr in attrs:
            value = getattr(self, attr)
            args.append('{}={}'.format(attr, repr(value)))

        return '{}({})'.format(type(self).__name__, ', '.join(args))

    def decorator(cls):
        setattr(cls, '__repr__', _repr)
        return cls

    return decorator
