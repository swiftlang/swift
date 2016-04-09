# tests/utils/test_argparser_builder.py -------------------------*- python -*-
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
from build_script.utils import ArgParserBuilder


class ArgParserBuilderTestCase(unittest.TestCase):

    def test_argparser_builder(self):
        builder = ArgParserBuilder()
        in_group = builder.in_group
        mutually_exclusive_group = builder.mutually_exclusive_group

        set_ = builder.set_action
        enable = builder.enable_action
        disable = builder.disable_action
        append = builder.append_action
        option = builder.add_option

        in_group("FooBar")

        option('--foo', set_(['foo1', 'foo2']))
        option(['-B', '--bar'], set_(const=True))
        option('--disable-baz', disable('baz', default=True))

        with mutually_exclusive_group():
            option('--test', enable('test'))
            option('--no-test', disable('test'))

        with mutually_exclusive_group():
            all_products = ['x_type', 'y_type']
            option('--typeA', set_(all_products, const="A"))
            option('--typeB', set_(all_products, const="B"))

        option('--compound',
               set_("compound_dest1", const="comp"),
               enable("compound_dest2"))

        option('--enabler1', enable(default=True))
        option('--enabler2', enable(default=True))
        option('--enabler3', enable(default=True))
        option('--disabler1', disable(default=False))
        option('--disabler2', disable(default=False))
        option('--disabler3', disable(default=False))

        option('--append1', append(separator=';'))
        option('--append2', append(join='=='))
        option('--append3', append(join=';', separator=","))

        parser = builder.build()
        args = parser.parse_args([
            '--foo', 'some', '--bar', '--disable-baz',
            '--no-test', '--typeA', '--compound',
            '--enabler2=0', '--enabler3=1',
            '--disabler2', '0', '--disabler3', '1',
            '--append1', 'foo;bar', '--append1', 'baz;qux',
            '--append2', 'foo,bar', '--append2', 'baz',
            '--append3', 'foo,bar', '--append3', 'baz',
        ])

        self.assertEqual(vars(args), {
            'foo1': 'some',
            'foo2': 'some',
            'bar': True,
            'baz': False,
            'test': False,
            'x_type': 'A',
            'y_type': 'A',
            'compound_dest1': "comp",
            'compound_dest2': True,
            'enabler1': True,
            'enabler2': False,
            'enabler3': True,
            'disabler1': False,
            'disabler2': True,
            'disabler3': False,
            'append1': ('foo', 'bar', 'baz', 'qux'),
            'append2': 'foo,bar==baz',
            'append3': 'foo;bar;baz'
        })

    def test_args(self):
        builder = ArgParserBuilder()

        builder.set_defaults(baz=12)
        builder.set_defaults(['test', 'test2'], 'swift')

        builder.add_option('--foo', builder.set_action)
        builder.add_option('--bar', builder.set_action('baz_value'))
        args = builder.build().parse_args(['--bar', 'Swift'])

        self.assertIs(args.baz, 12)
        self.assertIs(args.test, 'swift')
        self.assertIs(args.test2, 'swift')
        self.assertIs(args.foo, None)
        self.assertIs(args.baz_value, 'Swift')
