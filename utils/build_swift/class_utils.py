# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Class utility functions and decorators.
"""

from __future__ import absolute_import, unicode_literals


__all__ = []


def add_metaclass(metacls):
    def wrapper(cls):
        body = vars(cls).copy()

        body.pop('__dict__', None)
        body.pop('__weakref__', None)

        return metacls(cls.__name__, cls.__bases__, body)

    return wrapper


def enum():
    pass


def infer_repr():
    def wrapper(cls):
        pass