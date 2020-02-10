# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import unittest

from build_swift.argparse import (
    ArgumentParser, BoolType, Nargs, PathType, SUPPRESS, actions)

import six

from ... import utils


# -----------------------------------------------------------------------------

class TestAction(unittest.TestCase):

    def test_default_attributes(self):
        action = actions.Action(['--foo'], dests=['foo'])

        self.assertEqual(action.option_strings, ['--foo'])
        self.assertEqual(action.dests, ['foo'])
        self.assertEqual(action.const, None)
        self.assertEqual(action.default, None)
        self.assertEqual(action.type, None)
        self.assertEqual(action.choices, None)
        self.assertEqual(action.required, False)
        self.assertEqual(action.help, None)
        self.assertEqual(action.metavar, None)
        self.assertEqual(action.dest, SUPPRESS)

    def test_single_destination(self):
        action = actions.Action(['--foo'], 'foo')

        self.assertEqual(action.dests, ['foo'])

    def test_multiple_destinations(self):
        action = actions.Action(['--foo'], ['foo', 'bar'])

        self.assertEqual(action.dests, ['foo', 'bar'])

    def test_supports_dest_argument(self):
        action = actions.Action([], [], dest='foo')

        self.assertEqual(action.dest, SUPPRESS)

    def test_call_not_implemented(self):
        action = actions.Action([], [])

        with self.assertRaises(NotImplementedError):
            action(None, None, None, None)


class TestAppendAction(unittest.TestCase):

    def test_default_attributes(self):
        action = actions.AppendAction(['--foo'], dests=['foo'])

        self.assertEqual(action.nargs, Nargs.SINGLE)
        self.assertEqual(action.default, [])

    def test_default_value(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.AppendAction)

        args = parser.parse_args([])
        self.assertEqual(args.foo, [])

    def test_append(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.AppendAction)

        args = parser.parse_args(['--foo', 'bar', '--foo', 'baz'])
        self.assertEqual(args.foo, ['bar', 'baz'])


class TestCustomCallAction(unittest.TestCase):

    def test_non_callable(self):
        with self.assertRaises(TypeError):
            actions.CustomCallAction('--foo', dests=['foo'], call_func=None)

    def test_custom_call_func(self):
        def test_func(action, parser, namespace, values, option_string=None):
            for dest in action.dests:
                setattr(namespace, dest, values)

        parser = ArgumentParser()
        parser.add_argument(
            '--foo',
            action=actions.CustomCallAction,
            call_func=test_func,
            nargs=Nargs.SINGLE)

        args = parser.parse_args(['--foo', 'boo'])

        self.assertEqual(args.foo, 'boo')


class TestStoreAction(unittest.TestCase):

    def test_default_attributes(self):
        action = actions.StoreAction(['--foo'], dests=['foo'], choices=['bar'])
        self.assertEqual(action.nargs, Nargs.OPTIONAL)

        action = actions.StoreAction(['--foo'], dests=['foo'], const='bar')
        self.assertEqual(action.nargs, Nargs.ZERO)

    def test_choices(self):
        parser = ArgumentParser()
        action = parser.add_argument(
            '--foo',
            action=actions.StoreAction,
            choices=['bar', 'baz'])

        self.assertEqual(action.nargs, Nargs.OPTIONAL)

        with utils.quiet_output(), self.assertRaises(SystemExit):
            parser.parse_args(['--foo', 'qux'])

        args = parser.parse_args(['--foo', 'bar'])

        self.assertEqual(args.foo, 'bar')

    def test_store_value(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.StoreAction)

        args = parser.parse_args(['--foo', 'bar'])

        self.assertEqual(args.foo, 'bar')

    def test_store_const(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.StoreAction, const='bar')

        args = parser.parse_args(['--foo'])

        self.assertEqual(args.foo, 'bar')

    def test_store_value_multiple_destinations(self):
        parser = ArgumentParser()
        parser.add_argument(
            '--foo',
            dests=['foo', 'bar'],
            action=actions.StoreAction)

        args = parser.parse_args(['--foo', 'baz'])

        self.assertEqual(args.foo, 'baz')
        self.assertEqual(args.bar, 'baz')

    def test_store_const_multiple_destinations(self):
        parser = ArgumentParser()
        parser.add_argument(
            '--foo',
            dests=['foo', 'bar'],
            action=actions.StoreAction,
            const='baz')

        args = parser.parse_args(['--foo'])

        self.assertEqual(args.foo, 'baz')
        self.assertEqual(args.bar, 'baz')


class TestStoreIntAction(unittest.TestCase):

    def test_default_attributes(self):
        action = actions.StoreIntAction(['--foo'], dests=['foo'])

        self.assertEqual(action.nargs, Nargs.SINGLE)
        self.assertEqual(action.type, int)
        self.assertEqual(action.metavar, 'N')

    def test_valid_int(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.StoreIntAction)

        for i in [0, 1, 42, -64]:
            args = parser.parse_args(['--foo', six.text_type(i)])
            self.assertEqual(args.foo, i)

    def test_invalid_int(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.StoreIntAction)

        for i in [0.0, True, 'bar']:
            with utils.quiet_output(), self.assertRaises(SystemExit):
                parser.parse_args(['--foo', six.text_type(i)])


class TestStoreTrueAction(unittest.TestCase):

    def test_default_attributes(self):
        action = actions.StoreTrueAction(['--foo'], dests=['foo'])

        self.assertEqual(action.nargs, Nargs.ZERO)
        self.assertEqual(action.const, True)
        self.assertEqual(action.default, False)

    def test_default_value(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.StoreTrueAction)

        args = parser.parse_args([])

        self.assertFalse(args.foo)

    def test_store_true(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.StoreTrueAction)

        args = parser.parse_args(['--foo'])

        self.assertTrue(args.foo)


class TestStoreFalseAction(unittest.TestCase):

    def test_default_attributes(self):
        action = actions.StoreFalseAction(['--foo'], dests=['foo'])

        self.assertEqual(action.nargs, Nargs.ZERO)
        self.assertEqual(action.const, False)
        self.assertEqual(action.default, True)

    def test_default_value(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.StoreFalseAction)

        args = parser.parse_args([])

        self.assertTrue(args.foo)

    def test_store_false(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.StoreFalseAction)

        args = parser.parse_args(['--foo'])

        self.assertFalse(args.foo)


class TestStorePathAction(unittest.TestCase):

    def test_default_attributes(self):
        action = actions.StorePathAction(['--foo'], dests=['foo'])

        self.assertEqual(action.nargs, Nargs.SINGLE)
        self.assertIsInstance(action.type, PathType)

    def test_exists(self):
        action = actions.StorePathAction(['--foo'], dests=['foo'], exists=True)

        self.assertTrue(action.type._assert_exists)

    def test_executable(self):
        action = actions.StorePathAction(['--foo'], dests=['foo'],
                                         executable=True)

        self.assertTrue(action.type._assert_executable)


class TestToggleTrueAction(unittest.TestCase):

    def test_default_attributes(self):
        action = actions.ToggleTrueAction(['--foo'], dests=['foo'])

        self.assertEqual(action.nargs, Nargs.OPTIONAL)
        self.assertEqual(action.on_value, True)
        self.assertEqual(action.off_value, False)
        self.assertEqual(action.metavar, 'BOOL')

    def test_default_value(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.ToggleTrueAction)

        args = parser.parse_args([])

        self.assertFalse(args.foo)

    def test_with_no_arg(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.ToggleTrueAction)

        args = parser.parse_args(['--foo'])

        self.assertTrue(args.foo)

    def test_with_optional_true_arg(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.ToggleTrueAction)

        for value in BoolType.TRUE_VALUES:
            args = parser.parse_args(['--foo', six.text_type(value)])
            self.assertTrue(args.foo)

            args = parser.parse_args(['--foo={}'.format(value)])
            self.assertTrue(args.foo)

    def test_with_optional_false_arg(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.ToggleTrueAction)

        for value in BoolType.FALSE_VALUES:
            args = parser.parse_args(['--foo', six.text_type(value)])
            self.assertFalse(args.foo)

            args = parser.parse_args(['--foo={}'.format(value)])
            self.assertFalse(args.foo)

    def test_last_wins(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.ToggleTrueAction)

        args = parser.parse_args(['--foo=TRUE', '--foo', 'FALSE'])
        self.assertFalse(args.foo)

        args = parser.parse_args(['--foo=FALSE', '--foo'])
        self.assertTrue(args.foo)

        args = parser.parse_args(['--foo=FALSE', '--foo', 'TRUE'])
        self.assertTrue(args.foo)


class TestToggleFalseAction(unittest.TestCase):

    def test_default_attributes(self):
        action = actions.ToggleFalseAction(['--foo'], dests=['foo'])

        self.assertEqual(action.nargs, Nargs.OPTIONAL)
        self.assertEqual(action.on_value, False)
        self.assertEqual(action.off_value, True)
        self.assertEqual(action.metavar, 'BOOL')

    def test_default_value(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.ToggleFalseAction)

        args = parser.parse_args([])

        self.assertTrue(args.foo)

    def test_with_no_arg(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.ToggleFalseAction)

        args = parser.parse_args(['--foo'])

        self.assertFalse(args.foo)

    def test_with_optional_true_arg(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.ToggleFalseAction)

        for value in BoolType.TRUE_VALUES:
            args = parser.parse_args(['--foo', six.text_type(value)])
            self.assertFalse(args.foo)

            args = parser.parse_args(['--foo={}'.format(value)])
            self.assertFalse(args.foo)

    def test_with_optional_false_arg(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.ToggleFalseAction)

        for value in BoolType.FALSE_VALUES:
            args = parser.parse_args(['--foo', six.text_type(value)])
            self.assertTrue(args.foo)

            args = parser.parse_args(['--foo={}'.format(value)])
            self.assertTrue(args.foo)

    def test_last_wins(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.ToggleFalseAction)

        args = parser.parse_args(['--foo=TRUE', '--foo', 'FALSE'])
        self.assertTrue(args.foo)

        args = parser.parse_args(['--foo=FALSE', '--foo'])
        self.assertFalse(args.foo)

        args = parser.parse_args(['--foo=FALSE', '--foo', 'TRUE'])
        self.assertFalse(args.foo)


class TestUnuspportedAction(unittest.TestCase):

    def test_default_attributes(self):
        action = actions.UnsupportedAction(['--foo'])

        self.assertEqual(action.dests, [])
        self.assertEqual(action.nargs, Nargs.ZERO)
        self.assertEqual(action.default, SUPPRESS)
        self.assertEqual(action.message, None)

    def test_suppressed_default_value(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.UnsupportedAction)

        args = parser.parse_args([])

        self.assertFalse(hasattr(args, 'foo'))

    def test_raises_parser_error(self):
        parser = ArgumentParser()
        parser.add_argument('--foo', action=actions.UnsupportedAction)

        with utils.quiet_output(), self.assertRaises(SystemExit):
            parser.parse_args(['--foo'])

    def test_custom_error_message(self):
        message = 'custom error message'

        parser = ArgumentParser()
        action = parser.add_argument(
            '--foo',
            action=actions.UnsupportedAction,
            message=message)

        self.assertEqual(action.message, message)

        with utils.redirect_stderr() as stderr, self.assertRaises(SystemExit):
            parser.parse_args(['--foo'])

            self.assertIn(message, stderr)
