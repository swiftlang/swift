# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


__all__ = [
    'repr_class',
]


def repr_class(cls, args):
    """Helper function for implementing __repr__ methods on classes.
    """

    _args = []
    for key, value in args.items():
        _args.append('{}={}'.format(key, repr(value)))

    return '{}({})'.format(type(cls).__name__, ', '.join(_args))
