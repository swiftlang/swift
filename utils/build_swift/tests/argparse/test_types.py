# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


import os.path

from ..utils import TestCase
from ...argparse import ArgumentTypeError, types


# -----------------------------------------------------------------------------

class TestBoolType(TestCase):

    def test_true_values(self):
        bool_type = types.BoolType()

        self.assertTrue(bool_type(True))
        self.assertTrue(bool_type(1))
        self.assertTrue(bool_type('TRUE'))
        self.assertTrue(bool_type('True'))
        self.assertTrue(bool_type('true'))
        self.assertTrue(bool_type('1'))

    def test_false_values(self):
        bool_type = types.BoolType()

        self.assertFalse(bool_type(False))
        self.assertFalse(bool_type(0))
        self.assertFalse(bool_type('FALSE'))
        self.assertFalse(bool_type('False'))
        self.assertFalse(bool_type('false'))
        self.assertFalse(bool_type('0'))

    def test_custom_true_values(self):
        bool_type = types.BoolType(true_values=['TRUE', 'ON', '1'])

        self.assertTrue(bool_type('TRUE'))
        self.assertTrue(bool_type('ON'))
        self.assertTrue(bool_type('1'))

        self.assertRaises(ArgumentTypeError, bool_type, True)
        self.assertRaises(ArgumentTypeError, bool_type, 1)
        self.assertRaises(ArgumentTypeError, bool_type, 'True')
        self.assertRaises(ArgumentTypeError, bool_type, 'true')

    def test_custom_false_values(self):
        bool_type = types.BoolType(false_values=['FALSE', 'OFF', '0'])

        self.assertFalse(bool_type('FALSE'))
        self.assertFalse(bool_type('OFF'))
        self.assertFalse(bool_type('0'))

        self.assertRaises(ArgumentTypeError, bool_type, False)
        self.assertRaises(ArgumentTypeError, bool_type, 0)
        self.assertRaises(ArgumentTypeError, bool_type, 'False')
        self.assertRaises(ArgumentTypeError, bool_type, 'false')

    def test_invalid_values(self):
        bool_type = types.BoolType()

        self.assertRaises(ArgumentTypeError, bool_type, None)
        self.assertRaises(ArgumentTypeError, bool_type, 2)
        self.assertRaises(ArgumentTypeError, bool_type, 2.71828)
        self.assertRaises(ArgumentTypeError, bool_type, 'Invalid')


class TestPathType(TestCase):

    def setUp(self):
        self.home_dir = os.path.expanduser('~')

    def test_expands_path(self):
        path_type = types.PathType()

        path = path_type('/some/random/path/../')
        self.assertEqual('/some/random', path)

        path = path_type('~/path/to/some/file.txt')
        self.assertEqual(self.home_dir + '/path/to/some/file.txt', path)

        path = path_type('~/path/to/some/../file.txt')
        self.assertEqual(self.home_dir + '/path/to/file.txt', path)

    def test_assert_exists(self):
        path_type = types.PathType(assert_exists=True)

        with self.assertNotRaises(AssertionError):
            path_type(__file__)

        with self.assertRaises(AssertionError):
            path_type('/nonsensisal/path/')
            path_type('~/not-a-real/path to a file')


class TestRegexType(TestCase):

    def test_regex_match(self):
        regex_type = types.RegexType(r'a+b*')

        with self.assertNotRaises(ArgumentTypeError):
            regex_type('a')
            regex_type('aab')
            regex_type('abbbbbbb')

    def test_raises_argument_error(self):
        regex_type = types.RegexType(r'a+b*')

        with self.assertRaises(ArgumentTypeError):
            regex_type('')
            regex_type('b')
            regex_type('baaaa')


class TestClangVersionType(TestCase):

    def test_valid_clang_version(self):
        clang_version_type = types.ClangVersionType()

        with self.assertNotRaises(ArgumentTypeError):
            clang_version_type('1.0.0')
            clang_version_type('3.0.2.1')
            clang_version_type('200.0.56.3')
            clang_version_type('100000.0.0.1')

    def test_invalid_clang_version(self):
        clang_version_type = types.ClangVersionType()

        with self.assertRaises(ArgumentTypeError):
            clang_version_type('2')
            clang_version_type('3.0')
            clang_version_type('1.8.0.2')
            clang_version_type('100.0.56.1')


class TestSwiftVersionType(TestCase):

    def test_valid_swift_version(self):
        swift_version_type = types.SwiftVersionType()

        with self.assertNotRaises(ArgumentTypeError):
            swift_version_type('1.0')
            swift_version_type('3.0.2')
            swift_version_type('200.0.56')
            swift_version_type('100000.0.1')

    def test_invalid_swift_version(self):
        swift_version_type = types.SwiftVersionType()

        with self.assertRaises(ArgumentTypeError):
            swift_version_type('2')
            swift_version_type('1.8.0.2')
            swift_version_type('100.0.56.1')


class TestShellSplitType(object):

    def test_split(self):
        shell_split_type = types.ShellSplitType()

        split = shell_split_type('-BAR="foo bar"')
        self.assertEqual(split, ['-BAR="foo bar"'])

        split = shell_split_type('-BAR="foo bar" -BAZ="foo,bar",-QUX 42')
        self.assertEqual(split, [
            '-BAR="foo bar"',
            '-BAZ="foo,bar"',
            '-QUX',
            '42',
        ])
