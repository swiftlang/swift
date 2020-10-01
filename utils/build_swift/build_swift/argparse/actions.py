# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Hierarchy of action classes which support multiple destinations, similar to the
default actions provided by the standard argparse.
"""


from __future__ import absolute_import, unicode_literals

import argparse
import copy

import six

from .types import BoolType, PathType


__all__ = [
    'Action',
    'Nargs',

    'AppendAction',
    'CustomCallAction',
    'StoreAction',
    'StoreIntAction',
    'StoreTrueAction',
    'StoreFalseAction',
    'StorePathAction',
    'ToggleTrueAction',
    'ToggleFalseAction',
    'UnsupportedAction',
]


# -----------------------------------------------------------------------------

class Nargs(object):
    """Container class holding valid values for the number of arguments
    actions can accept. Other possible values include integer literals.
    """

    ZERO = 0
    SINGLE = None
    OPTIONAL = argparse.OPTIONAL
    ZERO_OR_MORE = argparse.ZERO_OR_MORE
    ONE_OR_MORE = argparse.ONE_OR_MORE


# -----------------------------------------------------------------------------

class Action(argparse.Action):
    """Slightly modified version of the Action class from argparse which has
    support for multiple destinations available rather than just a single one.
    """

    def __init__(self,
                 option_strings,
                 dests=None,
                 nargs=Nargs.SINGLE,
                 const=None,
                 default=None,
                 type=None,
                 choices=None,
                 required=False,
                 help=None,
                 metavar=None,
                 dest=None):  # exists for compatibility purposes

        if dests is None and dest is None:
            raise TypeError('action must supply either dests or dest')

        dests = dests or dest
        if dests == argparse.SUPPRESS:
            dests = []
            metavar = metavar or ''
        elif isinstance(dests, six.string_types):
            dests = [dests]
            metavar = metavar or dests[0].upper()

        super(Action, self).__init__(
            option_strings=option_strings,
            dest=argparse.SUPPRESS,
            nargs=nargs,
            const=const,
            default=default,
            type=type,
            choices=choices,
            required=required,
            help=help,
            metavar=metavar)

        self.dests = dests

    def _get_kwargs(self):
        """Unofficial method used for pretty-printing out actions.
        """

        names = [
            'option_strings',
            'dests',
            'nargs',
            'const',
            'default',
            'type',
            'choices',
            'required',
            'help',
            'metavar',
        ]

        return [(name, getattr(self, name)) for name in names]

    def __call__(self, parser, namespace, values, option_string=None):
        raise NotImplementedError('__call__ not defined')


# -----------------------------------------------------------------------------

class AppendAction(Action):
    """Action that appends
    """

    def __init__(self, option_strings, join=None, **kwargs):

        kwargs['nargs'] = Nargs.SINGLE
        kwargs.setdefault('default', [])

        super(AppendAction, self).__init__(
            option_strings=option_strings,
            **kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        if isinstance(values, six.string_types):
            values = [values]

        for dest in self.dests:
            if getattr(namespace, dest) is None:
                setattr(namespace, dest, [])

            items = copy.copy(getattr(namespace, dest))
            items += values

            setattr(namespace, dest, items)


# -----------------------------------------------------------------------------

class CustomCallAction(Action):
    """Action that allows for instances to implement custom call functionality.
    The call_func must follow the same calling convention as implementing the
    __call__ method for an Action.
    """

    def __init__(self, option_strings, call_func, **kwargs):

        if not callable(call_func):
            raise TypeError('call_func must be callable')

        super(CustomCallAction, self).__init__(
            option_strings=option_strings,
            **kwargs)

        self.call_func = call_func

    def __call__(self, parser, namespace, values, option_string=None):
        self.call_func(self, parser, namespace, values, option_string)


# -----------------------------------------------------------------------------

class StoreAction(Action):
    """Action that stores a string.
    """

    def __init__(self, option_strings, **kwargs):

        if 'choices' in kwargs:
            kwargs['nargs'] = Nargs.OPTIONAL
        elif 'const' in kwargs:
            kwargs['nargs'] = Nargs.ZERO

        super(StoreAction, self).__init__(
            option_strings=option_strings,
            **kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        for dest in self.dests:
            if self.nargs == Nargs.ZERO and self.const is not None:
                values = self.const

            setattr(namespace, dest, values)


class StoreIntAction(StoreAction):
    """Action that stores an int.
    """

    def __init__(self, option_strings, **kwargs):
        kwargs['nargs'] = Nargs.SINGLE
        kwargs['type'] = int
        kwargs.setdefault('metavar', 'N')

        super(StoreAction, self).__init__(
            option_strings=option_strings,
            **kwargs)


class StoreTrueAction(StoreAction):
    """Action that stores True when called and False by default.
    """

    def __init__(self, option_strings, **kwargs):
        kwargs['nargs'] = Nargs.ZERO
        kwargs['const'] = True
        kwargs['default'] = False

        super(StoreTrueAction, self).__init__(
            option_strings=option_strings,
            **kwargs)


class StoreFalseAction(StoreAction):
    """Action that stores False when called and True by default.
    """

    def __init__(self, option_strings, **kwargs):
        kwargs['nargs'] = Nargs.ZERO
        kwargs['const'] = False
        kwargs['default'] = True

        super(StoreFalseAction, self).__init__(
            option_strings=option_strings,
            **kwargs)


class StorePathAction(StoreAction):
    """Action that stores a path, which it will attempt to expand.
    """

    def __init__(self, option_strings, **kwargs):
        assert_exists = kwargs.pop('exists', False)
        assert_executable = kwargs.pop('executable', False)

        kwargs['nargs'] = Nargs.SINGLE
        kwargs['type'] = PathType(assert_exists, assert_executable)
        kwargs.setdefault('metavar', 'PATH')

        super(StorePathAction, self).__init__(
            option_strings=option_strings,
            **kwargs)


# -----------------------------------------------------------------------------

class _ToggleAction(Action):
    """Action that can be toggled on or off, either implicitly when called or
    explicitly when an optional boolean value is parsed.
    """

    def __init__(self, option_strings, on_value, off_value, **kwargs):
        kwargs['nargs'] = Nargs.OPTIONAL
        kwargs['type'] = BoolType()
        kwargs.setdefault('default', off_value)
        kwargs.setdefault('metavar', 'BOOL')

        super(_ToggleAction, self).__init__(
            option_strings=option_strings,
            **kwargs)

        self.on_value = on_value
        self.off_value = off_value

    def __call__(self, parser, namespace, values, option_string=None):
        if values is None:
            values = self.on_value
        elif values is True:
            values = self.on_value
        elif values is False:
            values = self.off_value
        else:
            raise argparse.ArgumentTypeError(
                '{} is not a boolean value'.format(values))

        for dest in self.dests:
            setattr(namespace, dest, values)


class ToggleTrueAction(_ToggleAction):
    """Action that toggles True when called or False otherwise, with the
    option to explicitly override the value.
    """

    def __init__(self, option_strings, **kwargs):

        super(ToggleTrueAction, self).__init__(
            option_strings=option_strings,
            on_value=True,
            off_value=False,
            **kwargs)


class ToggleFalseAction(_ToggleAction):
    """Action that toggles False when called or True otherwise, with the
    option to explicitly override the value.
    """

    def __init__(self, option_strings, **kwargs):

        super(ToggleFalseAction, self).__init__(
            option_strings=option_strings,
            on_value=False,
            off_value=True,
            **kwargs)


# -----------------------------------------------------------------------------

class UnsupportedAction(Action):
    """Action that denotes an unsupported argument or opiton and subsequently
    raises an ArgumentError.
    """

    def __init__(self, option_strings, message=None, **kwargs):
        kwargs['nargs'] = Nargs.ZERO
        kwargs['default'] = argparse.SUPPRESS

        super(UnsupportedAction, self).__init__(
            option_strings=option_strings,
            dests=argparse.SUPPRESS,
            **kwargs)

        self.message = message

    def __call__(self, parser, namespace, values, option_string=None):
        if self.message is not None:
            parser.error(self.message)

        arg = option_string or six.text_type(values)
        parser.error('unsupported argument: {}'.format(arg))
