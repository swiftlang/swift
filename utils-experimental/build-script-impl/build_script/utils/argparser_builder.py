# build_script/utils/argparser_builder.py -----------------------*- python -*-
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
"""
DSL like argparse.ArgumentParser builder
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

from contextlib import contextmanager
import argparse


def select_action_factory(choice1, choice2, choices=None):
    class _SelectAction(argparse.Action):
        '''Action that select `%s`  `%s`
        ''' % (choice1, choice2)

        def __init__(self, destgroup=None, **kwargs):
            kwargs['nargs'] = '?'
            if choices is not None:
                kwargs['choices'] = choices
            else:
                kwargs['metavar'] = "BOOL"
            super(_SelectAction, self).__init__(**kwargs)
            self.destgroup = destgroup

        def __call__(self, parser, namespace, values, option_string=None):
            if values not in [None, '0', 'False', 'false', 0, False]:
                val = choice2
            else:
                val = choice1

            dests = self.destgroup
            if dests is None:
                dests = [self.dest, ]
            for dest in dests:
                setattr(namespace, dest, val)
    return _SelectAction


class _SetAction(argparse.Action):
    '''Action that set an string value, or arbitrary constant.
    '''

    def __init__(self, destgroup=None, const=None, choices=None, **kwargs):
        assert const is None or choices is None, (
            'const and choices are mutually exclusive for set action')

        if const is not None:
            kwargs['nargs'] = 0

        super(_SetAction, self).__init__(
            const=const, choices=choices, **kwargs)
        self.destgroup = destgroup

    def __call__(self, parser, namespace, values, option_string=None):
        if self.const is not None:
            values = self.const
        dests = self.destgroup
        if dests is None:
            dests = [self.dest, ]
        for dest in dests:
                setattr(namespace, dest, values)


class _AppendAction(argparse.Action):
    '''Action that append specified string to a list.
    '''
    def __init__(self, destgroup=None,
                 join=None, separator=None, **kwargs):
        kwargs['nargs'] = 1
        if kwargs.get('default', None) is None:
            if join is not None:
                kwargs['default'] = ""
            else:
                kwargs['default'] = []
        super(_AppendAction, self).__init__(**kwargs)

        self.join = join
        self.separator = separator
        self.destgroup = destgroup

    def __call__(self, parser, namespace, values, option_string=None):
        dests = self.destgroup
        if dests is None:
            dests = [self.dest, ]

        if self.join is not None:
            append = self.join + values
        else:
            append = values

        for dest in dests:
            value = getattr(namespace, dest) + append
            setattr(namespace, dest, value)


class ArgParserBuilder(object):
    '''DSL like custom ArgParser builder.

    >>> builder = ArgParserBuilder()
    >>> in_group = builder.in_group
    >>> mutually_exclusive_group = builder.mutually_exclusive_group
    >>> set_ = builder.set_action
    >>> enable = builder.enable_action
    >>> disable = builder.disable_action
    >>> option = builder.add_option
    >>>
    >>> in_group("FooBar")
    >>>
    >>> option('foo', set_, ['foo1', 'foo2'])
    >>> option(['-B', 'bar'], set_, 'bar', as_=True)
    >>> option('disable-baz', disable, 'baz', default=True)
    >>>
    >>> with mutually_exclusive_group():
    ...     option('test', enable, 'test')
    ...     option('no-test', disable, 'test')
    ...
    >>> with mutually_exclusive_group():
    ...     all_products = ['x_type', 'y_type']
    ...     option('typeA', set_, all_products, as_="A")
    ...     option('typeB', set_, all_products, as_="B")
    ...
    >>> parser = builder.build()
    >>> args = parser.parse_args([
    ...     '--foo', 'some', '--bar', '--disable-baz',
    ...     '--no-test', '--typeA'])
    >>> vars(args) == {
    ...     'foo1': 'some',
    ...     'foo2': 'some',
    ...     'bar': True,
    ...     'baz': False,
    ...     'test': False,
    ...     'x_type': 'A',
    ...     'y_type': 'A'}
    True
    '''

    def __init__(self, **kwargs):
        self._parser = argparse.ArgumentParser(**kwargs)
        self._current_group = self._parser
        self._defaults = {}

        self.select_action_factory = select_action_factory
        self.enable_action = select_action_factory(True, False)
        self.disable_action = select_action_factory(False, True)
        self.set_action = _SetAction
        self.append_action = _AppendAction

    def build(self):
        self._parser.set_defaults(**self._defaults)
        return self._parser

    def in_group(self, description):
        self._current_group = self._parser.add_argument_group(description)

    def reset_group(self):
        self._current_group = self._parser

    @contextmanager
    def mutually_exclusive_group(self):
        old = self._current_group
        self._current_group = old.add_mutually_exclusive_group()
        yield
        self._current_group = old

    def set_defaults(self, *args, **kwargs):
        if len(args):
            assert len(args) == 2, "Invalid set_default argument."
            default_value = args[1]
            if isinstance(args[0], basestring):
                kwargs[args[0]] = default_value
            else:
                for name in args[0]:
                    kwargs[name] = default_value

        # Defaults will be added to the parser at build() time.
        self._defaults.update(kwargs)

    def add_option(
            self, names, action, dest=None, as_=None, from_=None,
            default=None, metavar=None, **kwargs):

        # Prevent accidentally use builtin `set`.
        assert action != set, (
            "built-in `set` is specified as action. Do you mean `set_`?")

        if isinstance(names, basestring):
            names = (names, )
        assert all(name.startswith("-") for name in names), (
            "add_option can't add positional arguments")

        destgroup = None
        if dest is not None and not isinstance(dest, basestring):
            assert default is None, (
                "Can't use default value for multiple destination.")
            destgroup = dest
            dest = argparse.SUPPRESS
            default = argparse.SUPPRESS
            if metavar is None:
                metavar = 'VALUE'

        if as_ is None and "constant" in kwargs:
            as_ = kwargs.pop("constant")

        if from_ is None and "choices" in kwargs:
            from_ = kwargs.pop("choices")

        self._current_group.add_argument(
            *names, action=action, dest=dest, destgroup=destgroup,
            default=default, const=as_, choices=from_,
            metavar=metavar, **kwargs)

    def add_positional_argument(self, dest, **kwargs):
        self._current_group.add_argument(
            dest, **kwargs)
