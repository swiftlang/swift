# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


from __future__ import absolute_import, unicode_literals

import unittest
from argparse import _ArgumentGroup, _MutuallyExclusiveGroup

from build_swift.argparse import ArgumentParser, actions

from ... import utils


# -----------------------------------------------------------------------------

class TestBuilder(unittest.TestCase):

    def test_build(self):
        builder = ArgumentParser.builder()

        self.assertEqual(builder._parser, builder.build())

    def test_add_positional(self):
        # Setup builder and DSL
        builder = ArgumentParser.builder()
        positional = builder.add_positional

        store = builder.actions.store
        store_int = builder.actions.store_int

        # Create test parser
        positional('foo', store)
        positional('bar', store_int(['bar', 'baz']))

        parser = builder.build()

        args = parser.parse_args(['Foo', '1'])

        self.assertEqual(args.foo, 'Foo')
        self.assertEqual(args.bar, 1)
        self.assertEqual(args.baz, 1)

    def test_add_option(self):
        # Setup builder and DSL
        builder = ArgumentParser.builder()
        option = builder.add_option

        append = builder.actions.append
        store_true = builder.actions.store_true
        toggle_false = builder.actions.toggle_false
        unsupported = builder.actions.unsupported

        # Create test parser
        option('--foo', append)
        option('--bar', store_true(['bar', 'foobar']))
        option('--baz', toggle_false)
        option('--qux', unsupported)

        parser = builder.build()

        args = parser.parse_args([])
        self.assertEqual(args.foo, [])
        self.assertFalse(args.bar)
        self.assertFalse(args.foobar)
        self.assertTrue(args.baz)
        self.assertFalse(hasattr(args, 'qux'))

        args = parser.parse_args(['--foo', 'Foo'])
        self.assertEqual(args.foo, ['Foo'])

        args = parser.parse_args(['--bar'])
        self.assertTrue(args.bar)
        self.assertTrue(args.foobar)

        args = parser.parse_args(['--baz', '--baz=FALSE'])
        self.assertTrue(args.baz)

        with utils.quiet_output(), self.assertRaises(SystemExit):
            parser.parse_args(['--qux'])

    def test_set_defaults(self):
        builder = ArgumentParser.builder()

        builder.set_defaults(foo=True, bar=False, baz=[])
        builder.set_defaults('qux', 'Lorem ipsum set dolor')

        self.assertEqual(builder._defaults, {
            'foo': True,
            'bar': False,
            'baz': [],
            'qux': 'Lorem ipsum set dolor',
        })

    def test_in_group(self):
        builder = ArgumentParser.builder()
        self.assertEqual(builder._current_group, builder._parser)

        group = builder.in_group('First Group')
        self.assertEqual(group, builder._current_group)
        self.assertNotEqual(group, builder._parser)

    def test_reset_group(self):
        builder = ArgumentParser.builder()
        self.assertEqual(builder._current_group, builder._parser)

        group = builder.in_group('First Group')
        builder.reset_group()
        self.assertNotEqual(group, builder._current_group)
        self.assertEqual(builder._current_group, builder._parser)

    def test_argument_group(self):
        builder = ArgumentParser.builder()

        with builder.argument_group('First Group') as group:
            self.assertEqual(builder._current_group, group)
            self.assertIsInstance(group, _ArgumentGroup)

        self.assertEqual(builder._current_group, builder._parser)

    def test_mutually_exclusive_group(self):
        builder = ArgumentParser.builder()

        with builder.mutually_exclusive_group(required=True) as group:
            self.assertEqual(builder._current_group, group)
            self.assertIsInstance(group, _MutuallyExclusiveGroup)
            self.assertTrue(group.required)

        self.assertEqual(builder._current_group, builder._parser)


class TestArgumentParser(unittest.TestCase):

    def test_builder(self):
        builder = ArgumentParser.builder(usage='Totally useless help message')

        self.assertIsInstance(builder._parser, ArgumentParser)
        self.assertEqual(builder._parser.usage, 'Totally useless help message')

    def test_builder_uses_subclass(self):
        class _ArgumentParserSubclass(ArgumentParser):
            pass

        builder = _ArgumentParserSubclass.builder()

        self.assertIsInstance(builder._parser, _ArgumentParserSubclass)

    def test_to_builder(self):
        parser = ArgumentParser()
        builder = parser.to_builder()

        self.assertEqual(parser, builder._parser)

    def test_parse_known_args_adds_defaults_to_dests(self):
        parser = ArgumentParser()
        parser.add_argument(
            '--foo',
            action=actions.StoreAction,
            dests=['foo', 'bar'],
            default='FooBar')

        # parse_args calls parse_known_args under the hood
        args = parser.parse_args([])
        self.assertEqual(args.foo, 'FooBar')
        self.assertEqual(args.bar, 'FooBar')
