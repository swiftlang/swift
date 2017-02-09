# tests/test_cache_util.py --------------------------------------*- python -*-
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

import unittest

from swift_build_support import cache_util


my_func_called = 0
my_kfunc_called = 0


@cache_util.cached
def my_func(arg1, arg2):
    global my_func_called
    my_func_called += 1
    return "my_func_result(%s, %s)" % (arg1, arg2)


@cache_util.cached
def my_kfunc(arg1, arg2):
    global my_kfunc_called
    my_kfunc_called += 1
    return "my_kfunc_result(%s, %s)" % (arg1, arg2)


class MyClass(object):
    def __init__(self, prop=None):
        self.my_method_called = 0
        self.my_prop_called = 0
        self.prop_value = prop

    @cache_util.cached
    def my_method(self, arg1, arg2):
        self.my_method_called += 1
        return "my_meth_result(%s, %s)" % (arg1, arg2)

    @cache_util.reify
    def my_prop(self):
        self.my_prop_called += 1
        return "==%s==" % (self.prop_value)


class CacheUtilTestCase(unittest.TestCase):
    def test_cached_func(self):
        self.assertEqual(my_func("foo", 42), "my_func_result(foo, 42)")
        self.assertEqual(my_func_called, 1)
        self.assertEqual(my_func("foo", 42), "my_func_result(foo, 42)")
        self.assertEqual(my_func_called, 1)
        self.assertEqual(my_func("bar", 42), "my_func_result(bar, 42)")
        self.assertEqual(my_func_called, 2)
        self.assertEqual(my_func("foo", 42), "my_func_result(foo, 42)")
        self.assertEqual(my_func_called, 2)

    def test_cached_kwfunc(self):
        self.assertEqual(my_kfunc("foo", arg2=42), "my_kfunc_result(foo, 42)")
        self.assertEqual(my_kfunc_called, 1)
        self.assertEqual(my_kfunc("foo", arg2=42), "my_kfunc_result(foo, 42)")
        self.assertEqual(my_kfunc_called, 1)
        self.assertEqual(my_kfunc("bar", arg2=42), "my_kfunc_result(bar, 42)")
        self.assertEqual(my_kfunc_called, 2)
        self.assertEqual(my_kfunc("foo", arg2=42), "my_kfunc_result(foo, 42)")
        self.assertEqual(my_kfunc_called, 2)

    def test_cached_method(self):
        obj1 = MyClass()
        self.assertEqual(obj1.my_method("foo", 42), "my_meth_result(foo, 42)")
        self.assertEqual(obj1.my_method_called, 1)
        self.assertEqual(obj1.my_method("foo", 42), "my_meth_result(foo, 42)")
        self.assertEqual(obj1.my_method_called, 1)
        self.assertEqual(obj1.my_method("bar", 12), "my_meth_result(bar, 12)")
        self.assertEqual(obj1.my_method_called, 2)

        # Test for instance independency.
        obj2 = MyClass()
        self.assertEqual(obj2.my_method("foo", 42), "my_meth_result(foo, 42)")
        self.assertEqual(obj2.my_method_called, 1)
        self.assertEqual(obj1.my_method_called, 2)

    def test_reify(self):
        obj1 = MyClass(prop='foo')
        self.assertEqual(obj1.my_prop, '==foo==')
        self.assertEqual(obj1.my_prop_called, 1)
        self.assertEqual(obj1.my_prop, '==foo==')
        self.assertEqual(obj1.my_prop_called, 1)

        # Test for instance independency.
        obj2 = MyClass(prop='bar')
        self.assertEqual(obj2.my_prop, '==bar==')
        self.assertEqual(obj1.my_prop, '==foo==')
