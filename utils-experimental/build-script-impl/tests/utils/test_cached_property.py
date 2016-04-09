# tests/utils/test_cached_property.py ---------------------------*- python -*-
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

import unittest

from build_script.utils import CachedProperty


i = 0


class Foo(object):
    @CachedProperty
    def bar(self):
        global i
        i += 1
        return i


class CachedPropertyTestCase(unittest.TestCase):

    def test_cached_property(self):
        global i

        foo = Foo()
        ret = foo.bar
        self.assertEqual(i, 1)
        self.assertEqual(ret, 1)
        ret = foo.bar
        self.assertEqual(i, 1)
        self.assertEqual(ret, 1)

        i = 42
        ret = foo.bar
        self.assertEqual(ret, 1)
