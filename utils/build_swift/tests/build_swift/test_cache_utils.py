# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import unittest

from build_swift import cache_utils

from .. import utils


try:
    # Python 3.3
    from unittest import mock
except ImportError:
    mock = None


class _CallCounter(object):
    """Callable helper class used to count and return the number of times an
    instance has been called.
    """

    def __init__(self):
        self._counter = 0

    def __call__(self, *args, **kwargs):
        count = self._counter
        self._counter += 1
        return count


class TestCache(unittest.TestCase):
    """Unit tests for the cache decorator in the cache_utils module.
    """

    @utils.requires_module('unittest.mock')
    @utils.requires_python('3.2')  # functools.lru_cache
    def test_replaced_with_functools_lru_cache_python_3_2(self):
        with mock.patch('functools.lru_cache') as mock_lru_cache:
            @cache_utils.cache
            def func():
                return None

        assert(mock_lru_cache.called)

    def test_call_with_no_args(self):
        # Increments the counter once per unique call.
        counter = _CallCounter()

        @cache_utils.cache
        def func(*args, **kwargs):
            return counter(*args, **kwargs)

        self.assertEqual(func(), 0)
        self.assertEqual(func(), 0)

    def test_call_with_args(self):
        # Increments the counter once per unique call.
        counter = _CallCounter()

        @cache_utils.cache
        def func(*args, **kwargs):
            return counter(*args, **kwargs)

        self.assertEqual(func(0), 0)
        self.assertEqual(func(0), 0)

        self.assertEqual(func(1), 1)
        self.assertEqual(func(1), 1)

        self.assertEqual(func(2), 2)
        self.assertEqual(func(2), 2)

    def test_call_with_args_and_kwargs(self):
        # Increments the counter once per unique call.
        counter = _CallCounter()

        @cache_utils.cache
        def func(*args, **kwargs):
            return counter(*args, **kwargs)

        self.assertEqual(func(n=0), 0)
        self.assertEqual(func(n=0), 0)

        self.assertEqual(func(a=1, b='b'), 1)
        self.assertEqual(func(a=1, b='b'), 1)

        self.assertEqual(func(0, x=1, y=2.0), 2)
        self.assertEqual(func(0, x=1, y=2.0), 2)


class TestReify(unittest.TestCase):
    """Unit tests for the reify decorator in the cache_utils module.
    """

    def test_replaces_attr_after_first_call(self):
        class Counter(object):
            def __init__(self):
                self._counter = 0

            @cache_utils.reify
            def count(self):
                count = self._counter
                self._counter += 1
                return count

        counter = Counter()

        self.assertEqual(counter.count, 0)
        self.assertEqual(counter.count, 0)

        # Assert that the count property has been replaced with the constant.
        self.assertEqual(getattr(counter, 'count'), 0)
