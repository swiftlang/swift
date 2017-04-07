# tests/arguments.py --------------------------------------------*- python -*-
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

import argparse
import os
import sys
import unittest
try:
    # py2
    from StringIO import StringIO
except ImportError:
    # py3
    from io import StringIO

from swift_build_support.arguments import (
    action as argaction,
    type as argtype,
)


class ArgumentsTypeTestCase(unittest.TestCase):

    def test_bool(self):
        self.assertTrue(argtype.bool("1"))
        self.assertTrue(argtype.bool("true"))
        self.assertTrue(argtype.bool("True"))

        self.assertFalse(argtype.bool("0"))
        self.assertFalse(argtype.bool("false"))
        self.assertFalse(argtype.bool("False"))

        self.assertRaises(argparse.ArgumentTypeError, argtype.bool, 'foobar')
        self.assertRaises(argparse.ArgumentTypeError, argtype.bool, 'TRUE')
        self.assertRaises(argparse.ArgumentTypeError, argtype.bool, 'FALSE')

    def test_shell_split(self):
        self.assertEqual(
            argtype.shell_split("-BAR=\"foo bar\" -BAZ='foo,bar',-QUX $baz"),
            ['-BAR=foo bar', '-BAZ=foo,bar', '-QUX', '$baz'])

    def test_clang_compiler_version(self):
        self.assertEqual(
            argtype.clang_compiler_version('1.23.456').components,
            ("1", "23", "456", None))
        self.assertEqual(
            argtype.clang_compiler_version('1.2.3').components,
            ("1", "2", "3", None))
        self.assertEqual(
            argtype.clang_compiler_version('1.2.3.4').components,
            ("1", "2", "3", "4"))
        self.assertEqual(
            argtype.clang_compiler_version('12.34.56').components,
            ("12", "34", "56", None))
        self.assertEqual(
            argtype.clang_compiler_version('12.34.56.78').components,
            ("12", "34", "56", "78"))
        self.assertRaises(
            argparse.ArgumentTypeError,
            argtype.clang_compiler_version,
            "ver1.2.3")
        self.assertRaises(
            argparse.ArgumentTypeError,
            argtype.clang_compiler_version,
            "1.beta2.3")
        self.assertRaises(
            argparse.ArgumentTypeError,
            argtype.clang_compiler_version,
            "1.2.preview3")
        self.assertRaises(
            argparse.ArgumentTypeError,
            argtype.clang_compiler_version,
            "1.2.3-rc4")
        self.assertRaises(
            argparse.ArgumentTypeError,
            argtype.clang_compiler_version,
            "1..2")

    def test_executable(self):
        python = sys.executable
        self.assertTrue(os.path.isabs(argtype.executable(python)))

        # On this test directory, specifying "../../build-script-impl" returns
        # absolute path of build-script-impl
        impl = os.path.join("..", "..", "build-script-impl")
        cwd = os.getcwd()
        os.chdir(os.path.dirname(__file__))
        self.assertTrue(os.path.isabs(argtype.executable(impl)))
        os.chdir(cwd)

        self.assertRaises(
            argparse.ArgumentTypeError,
            argtype.executable, __file__)  # this file is not executable
        self.assertRaises(
            argparse.ArgumentTypeError,
            argtype.executable, os.path.dirname(__file__))
        self.assertRaises(
            argparse.ArgumentTypeError,
            argtype.executable, "/bin/example-command-not-exist")
        self.assertRaises(
            argparse.ArgumentTypeError,
            argtype.executable, "../example-command-not-exist")


class ArgumentsActionTestCase(unittest.TestCase):

    def test_unavailable(self):
        orig_stderr = sys.stderr

        parser = argparse.ArgumentParser()
        parser.add_argument("--foo")
        parser.add_argument(
            "--do-not-use",
            "--never-ever",
            action=argaction.unavailable)

        args, unknown_args = parser.parse_known_args(
            ['--foo', 'bar', '--baz', 'qux'])

        self.assertEqual(args.foo, 'bar')
        self.assertEqual(unknown_args, ['--baz', 'qux'])
        self.assertFalse(hasattr(args, 'sentinel'))

        stderr = StringIO()
        sys.stderr = stderr
        self.assertRaises(
            SystemExit,
            parser.parse_known_args,
            ['--foo', 'bar', '--do-not-use', 'baz'])
        self.assertIn('--do-not-use', stderr.getvalue())

        stderr = StringIO()
        sys.stderr = stderr
        self.assertRaises(
            SystemExit,
            parser.parse_known_args,
            ['--foo', 'bar', '--never-ever=baz'])
        self.assertIn('--never-ever', stderr.getvalue())

        sys.stderr = orig_stderr

    def test_concat(self):
        # Has default
        parser = argparse.ArgumentParser()
        parser.add_argument(
            "--str-opt",
            action=argaction.concat,
            default="def")
        parser.add_argument(
            "--list-opt",
            action=argaction.concat,
            type=argtype.shell_split,
            default=["def"])

        self.assertEqual(
            parser.parse_args(['--str-opt', '12', '--str-opt=42']),
            argparse.Namespace(str_opt="def1242", list_opt=["def"]))

        self.assertEqual(
            parser.parse_args(['--list-opt', 'foo 12', '--list-opt=bar 42']),
            argparse.Namespace(
                str_opt="def", list_opt=["def", "foo", "12", "bar", "42"]))

        # Default less
        parser = argparse.ArgumentParser()
        parser.add_argument(
            "--str-opt",
            action=argaction.concat)
        parser.add_argument(
            "--list-opt",
            action=argaction.concat,
            type=argtype.shell_split)

        self.assertEqual(
            parser.parse_args(['--str-opt', '12', '--str-opt=42']),
            argparse.Namespace(str_opt="1242", list_opt=None))

        self.assertEqual(
            parser.parse_args(['--list-opt', 'foo 12', '--list-opt=bar 42']),
            argparse.Namespace(
                str_opt=None, list_opt=["foo", "12", "bar", "42"]))

    def test_optional_bool(self):
        parser = argparse.ArgumentParser()
        parser.add_argument(
            "--test-default-default",
            action=argaction.optional_bool)
        parser.add_argument(
            "--test-default-true",
            action=argaction.optional_bool,
            default=True)
        parser.add_argument(
            "--test-default-false",
            action=argaction.optional_bool,
            default=False)

        args, unknown_args = parser.parse_known_args([])
        self.assertEqual(args.test_default_default, False)
        self.assertEqual(args.test_default_true, True)
        self.assertEqual(args.test_default_false, False)

        args, unknown_args = parser.parse_known_args(
            ['--test-default-default', '0',
             '--test-default-true', '0',
             '--test-default-false', '0'])
        self.assertEqual(args.test_default_default, False)
        self.assertEqual(args.test_default_true, False)
        self.assertEqual(args.test_default_false, False)

        args, unknown_args = parser.parse_known_args(
            ['--test-default-default', '1',
             '--test-default-true', '1',
             '--test-default-false', '1'])
        self.assertEqual(args.test_default_default, True)
        self.assertEqual(args.test_default_true, True)
        self.assertEqual(args.test_default_false, True)

        args, unknown_args = parser.parse_known_args(
            ['--test-default-default',
             '--test-default-true',
             '--test-default-false'])
        self.assertEqual(args.test_default_default, True)
        self.assertEqual(args.test_default_true, True)
        self.assertEqual(args.test_default_false, True)
