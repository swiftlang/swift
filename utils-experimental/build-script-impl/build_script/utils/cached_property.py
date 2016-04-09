# build_script/utils/cached_property.py -------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
"""
Cached property decorator
"""
# ----------------------------------------------------------------------------

class CachedProperty(object):
    """Decorator that lazy-loads the value of a property.

    The first time the property is accessed, the original property function is
    executed. The value it returns is set as the new value of that instance's
    property, replacing the original method.

    >>> var i = 0
    >>> class Foo(object):
    ...     @CachedProperty
    ...     def bar(self):
    ...         print("bar called")
    ...         i += 1
    ...         return i
    ...
    >>> foo = Foo()
    >>> ret1 = foo.bar
    bar called
    >>> assert isinstance(ret,  int) and ret == 1
    >>> ret2 = foo.bar
    >>> assert isinstance(ret,  int) and ret == 1
    """

    def __init__(self, wrapped):
        self.wrapped = wrapped
        try:
            self.__doc__ = wrapped.__doc__
        except AttributeError:
            pass

    def __get__(self, instance, instance_type=None):
        if instance is None:
            return self

        value = self.wrapped(instance)
        setattr(instance, self.wrapped.__name__, value)

        return value
