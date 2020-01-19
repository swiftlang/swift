# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import unittest

from build_swift.versions import Version

import six


class TestVersion(unittest.TestCase):
    """Unit tests for the Version class.
    """

    VERSION_COMPONENTS = {
        '': (),
        'a': ('a',),
        '0': (0,),
        'a0': ('a', 0),
        'a0b1': ('a', 0, 'b', 1),
        '1.0': (1, 0),
        '1.2.3': (1, 2, 3),
        'foo': ('foo',),
    }

    # -------------------------------------------------------------------------
    # Helpers

    def assertVersionEqual(self, v1, v2):
        v1 = Version(v1)
        v2 = Version(v2)

        self.assertEqual(v1, v2)
        self.assertEqual(v2, v1)

    def assertVersionNotEqual(self, v1, v2):
        v1 = Version(v1)
        v2 = Version(v2)

        self.assertNotEqual(v1, v2)
        self.assertNotEqual(v2, v1)

    def assertVersionLess(self, v1, v2):
        v1 = Version(v1)
        v2 = Version(v2)

        self.assertLess(v1, v2)
        self.assertGreater(v2, v1)
        self.assertNotEqual(v1, v2)

    # -------------------------------------------------------------------------

    def test_parse(self):
        for string, components in six.iteritems(self.VERSION_COMPONENTS):
            # Version parses
            version = Version(string)

            self.assertEqual(version.components, components)

    def test_equal(self):
        self.assertVersionEqual('', '')
        self.assertVersionEqual('a', 'a')
        self.assertVersionEqual('0', '0')
        self.assertVersionEqual('a0', 'a0')
        self.assertVersionEqual('1.0', '1.0')
        self.assertVersionEqual('foo', 'foo')

    def test_not_equal(self):
        self.assertVersionNotEqual('a', 'b')
        self.assertVersionNotEqual('0', '1')
        self.assertVersionNotEqual('a', '0')
        self.assertVersionNotEqual('0a', 'a0')
        self.assertVersionNotEqual('1.0', '1.1')
        self.assertVersionNotEqual('foo', 'bar')

    def test_less_than(self):
        self.assertVersionLess('0', '1')
        self.assertVersionLess('a', 'b')
        self.assertVersionLess('1.0', '1.1')
        self.assertVersionLess('1a', '1b')
        self.assertVersionLess('1aa', '1b')
        self.assertVersionLess('a0b', 'a1')

    def test_str(self):
        for string in six.iterkeys(self.VERSION_COMPONENTS):
            version = Version(string)

            self.assertEqual(six.text_type(version), string)
