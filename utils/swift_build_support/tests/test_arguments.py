# tests/arguments.py --------------------------------------------*- python -*-
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
    type as argtype,
    action as argaction,
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
            argtype.clang_compiler_version('1.23.456'),
            ("1", "23", "456"))
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
