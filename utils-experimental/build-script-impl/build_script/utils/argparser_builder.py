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


class _MultiDestinationAction(argparse.Action):
    '''Action that can have multiple destinations.
    '''
    def __init__(self, dests=None, **kwargs):

        if dests is None:
            pass
        elif isinstance(dests, str):
            kwargs['dest'] = dests
            dests = None
        else:
            # So that the default value not set to the inferred dest name.
            kwargs['dest'] = argparse.SUPPRESS

        super(_MultiDestinationAction, self).__init__(**kwargs)
        self.destgroup = dests

    def dests(self):
        dests = self.destgroup
        if dests is None:
            dests = [self.dest, ]
        return dests

    def __call__(self, parser, namespace, values, option_string=None):
        for dest in self.dests():
            setattr(namespace, dest, values)


class _OnOffAction(_MultiDestinationAction):
    def __init__(self, **kwargs):
        assert 'choices' in kwargs
        if 'metavar' not in kwargs:
            kwargs['metavar'] = "BOOL"
        self._vals = kwargs.pop('choices')
        kwargs['nargs'] = '?'
        super(_OnOffAction, self).__init__(**kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        if values is None:
            val = self._vals[0]
        elif values not in ['0', 'FALSE', 'False', 'false', 0, False]:
            val = self._vals[0]
        else:
            val = self._vals[1]
        super(_OnOffAction, self).__call__(
            parser, namespace, val, option_string=None)


class _EnableAction(_OnOffAction):
    def __init__(self, **kwargs):
        kwargs['choices'] = [True, False]
        kwargs['default'] = kwargs.get('default', False)
        super(_EnableAction, self).__init__(**kwargs)


class _DisableAction(_OnOffAction):
    def __init__(self, **kwargs):
        kwargs['choices'] = [False, True]
        kwargs['default'] = kwargs.get('default', True)
        super(_DisableAction, self).__init__(**kwargs)


class _AppendAction(_MultiDestinationAction):
    '''Action that append specified string to a list.
    '''

    def __init__(self, join=None, separator=None, **kwargs):
        kwargs['nargs'] = None
        if kwargs.get('default', None) is None:
            if join is not None:
                kwargs['default'] = ""
            else:
                kwargs['default'] = ()
        super(_AppendAction, self).__init__(**kwargs)

        self.join = join
        self.separator = separator

    def __call__(self, parser, namespace, values, option_string=None):
        if self.separator is not None:
            values = values.split(self.separator)
        else:
            values = [values, ]

        if self.join is not None:
            append = (self.join).join(values)
        else:
            append = tuple(values)

        for dest in self.dests():
            value = getattr(namespace, dest)
            if len(value) and self.join is not None:
                value = value + self.join
            value = value + append
            setattr(namespace, dest, value)


class _SetAction(_MultiDestinationAction):
    '''Action that set an string value, or arbitrary constant.
    '''

    def __init__(self, **kwargs):
        kwargs['const'] = kwargs.get('as_', kwargs.get('const', None))
        super(_SetAction, self).__init__(**kwargs)
        if kwargs['const'] is not None:
            self.nargs = 0

    def __call__(self, parser, namespace, value, option_string=None):
        if self.const is not None:
            value = self.const
        super(_SetAction, self).__call__(
            parser, namespace, value, option_string=None)


class _CompoundAction(argparse.Action):
    '''Action containing multiple actions.
    '''
    def __init__(self, actions, **kwargs):
        _actions = []
        for a in actions:
            _actions.append(a(**kwargs))

        if 'nargs' not in kwargs:
            kwargs['nargs'] = _actions[0].nargs
        if 'metavar' not in kwargs:
            kwargs['metavar'] = _actions[0].metavar
        if 'choices' not in kwargs:
            kwargs['choices'] = _actions[0].choices

        super(_CompoundAction, self).__init__(**kwargs)
        self.actions = _actions
        # Compound action should not have default inferred dest.
        self.dest = argparse.SUPPRESS

    def __call__(self, *args):
        for action in self.actions:
            action(*args)


class _action_partial(object):
    '''Partial to make option(flag, act) and option(flag, act()) equivalent.
    '''
    def __init__(self, action):
        self.action = action

    def __call__(self, dests=None, **kwargs):
        def factory(*args, **argparse_kwargs):
            argparse_kwargs.update(kwargs)  # Innner kwargs are stronger.
            return self.action(*args, dests=dests, **argparse_kwargs)
        return factory


class ArgParserBuilder(object):
    '''DSL like custom ArgParser builder.
    '''

    def __init__(self, **kwargs):
        self._parser = argparse.ArgumentParser(**kwargs)
        self._current_group = self._parser
        self._defaults = {}

        self.enable_action = _action_partial(_EnableAction)
        self.disable_action = _action_partial(_DisableAction)
        self.set_action = _action_partial(_SetAction)
        self.append_action = _action_partial(_AppendAction)

    def build(self):
        self._parser.set_defaults(**self._defaults)
        return self._parser

    def add_option(self, names, *actions, **kwargs):

        if isinstance(names, str):
            names = [names, ]

        assert all(name.startswith("-") for name in names), (
            "add_option can't add positional arguments")

        # Unwrap partial actions.
        _actions = []
        for act in actions:
            if isinstance(act, _action_partial):
                act = act()
            _actions.append(act)
        actions = _actions

        if len(actions) == 0:
            # add_option(name) -- equivalents to option(name, set_())
            action = _SetAction
        elif len(actions) == 1:
            # option(name, action)
            action = actions[0]
        else:
            def factory(*args, **kwargs):
                kwargs['actions'] = actions
                return _CompoundAction(*args, **kwargs)
            action = factory

        self._current_group.add_argument(*names, action=action, **kwargs)

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
            (names, default_value) = args
            if isinstance(names, str):
                names = [names, ]
            for name in names:
                kwargs[name] = default_value

        # Defaults will be added to the parser at build() time.
        self._defaults.update(kwargs)

    def add_positional_argument(self, dest, **kwargs):
        self._current_group.add_argument(
            dest, **kwargs)
