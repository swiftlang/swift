# swift_build_support/cache_util.py -----------------------------*- python -*-
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
Cache related utilities
"""
# ----------------------------------------------------------------------------

from functools import update_wrapper

__all__ = [
    'cached',
    'reify'
]


def cached(func):
    """Decorator that caches result of method or function.

    Note: Support method or function.
    """
    cache = {}

    def wrapper(*args, **kwargs):
        key = tuple(args) + tuple(kwargs.items())
        if key not in cache:
            result = func(*args, **kwargs)
            cache[key] = result
            return result
        else:
            return cache[key]

    return update_wrapper(wrapper, func)


def reify(func):
    """Decorator that replaces the wrapped method with the result after the
    first call.

    Note: Support method that takes no arguments.
    """
    class Wrapper(object):
        def __get__(self, obj, objtype=None):
            if obj is None:
                return self
            result = func(obj)
            setattr(obj, func.__name__, result)
            return result

    return update_wrapper(Wrapper(), func)
