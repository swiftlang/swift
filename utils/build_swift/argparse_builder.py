# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

"""
Extension classes to the standard argparse library allowing for a more
expressive and constraint-based argument definition.
"""


from contextlib import contextmanager

import argparse

from .argument_types import PathType


__all__ = [
    'ArgumentParserBuilder',
]


# -----------------------------------------------------------------------------
# Action Classes

class _UnsupportedAction(argparse.Action):
    """Action that is not supported.
    """

    def __init__(self, message=None, **kwargs):
        kwargs.setdefault('dest', argparse.SUPPRESS)
        kwargs.setdefault('default', argparse.SUPPRESS)
        kwargs.setdefault('nargs', argparse.OPTIONAL)

        self._message = message
        super(_UnsupportedAction, self).__init__(**kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        arg = option_string or str(values)

        if self._message is not None:
            parser.error(self._message)
        else:
            parser.error('unsupported argument: {}'.format(arg))


class _MultiDestinationAction(argparse.Action):
    """Action that can have multiple destinations.
    """

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
        self._dests = dests

    def dests(self):
        return self._dests or [self.dest]

    def __call__(self, parser, namespace, values, option_string=None):
        for dest in self.dests():
            setattr(namespace, dest, values)


class _SetAction(_MultiDestinationAction):
    """Action that sets destinations to either a constant value, an
    input string value or None.
    """

    def __init__(self, **kwargs):
        kwargs.setdefault('nargs', None)

        if 'choices' in kwargs:
            kwargs['nargs'] = argparse.OPTIONAL
        elif 'const' in kwargs:
            kwargs['nargs'] = 0

        super(_SetAction, self).__init__(**kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        if self.choices is None and self.const is not None:
            values = self.const

        super(_SetAction, self).__call__(
            parser, namespace, values, option_string)


class _SetIntAction(_SetAction):
    """Action that sets destinations to a valid int.
    """

    def __init__(self, **kwargs):
        kwargs['type'] = int
        kwargs.setdefault('metavar', 'N')
        super(_SetIntAction, self).__init__(**kwargs)


class _SetFloatAction(_SetAction):
    """Action that sets destinations to a valid float.
    """

    def __init__(self, **kwargs):
        kwargs['type'] = float
        kwargs.setdefault('metavar', 'N')
        super(_SetFloatAction, self).__init__(**kwargs)


class _SetPathAction(_SetAction):
    """Action that sets destinations to a valid path string.
    """

    def __init__(self, **kwargs):
        kwargs['type'] = PathType()
        kwargs.setdefault('metavar', 'PATH')
        super(_SetPathAction, self).__init__(**kwargs)


class _SetTrueAction(_SetAction):
    """Action that sets destinations to 'True' when parsed and 'False' by
    default.
    """

    def __init__(self, **kwargs):
        kwargs['const'] = True
        kwargs['default'] = False
        super(_SetTrueAction, self).__init__(**kwargs)


class _SetFalseAction(_SetAction):
    """Action that sets destinations to 'False' when parsed and 'True' by
    default.
    """

    def __init__(self, **kwargs):
        kwargs['const'] = False
        kwargs['default'] = True
        super(_SetFalseAction, self).__init__(**kwargs)


class _OnOffAction(_MultiDestinationAction):
    """Action that can be toggled on or off, defaulting to the off state. An
    optional bool-ish argument can be passed to set the state manually.
    """

    TRUE_VALUES = [1, True, '1', 'true', 'True', 'TRUE']
    FALSE_VALUES = [0, False, '0', 'false', 'False', 'FALSE']

    def __init__(self, **kwargs):
        assert 'on_value' in kwargs
        assert 'off_value' in kwargs

        self._on_value = kwargs.pop('on_value')
        self._off_value = kwargs.pop('off_value')
        kwargs['nargs'] = argparse.OPTIONAL

        kwargs.setdefault('default', self._off_value)
        kwargs.setdefault('metavar', 'BOOL')

        super(_OnOffAction, self).__init__(**kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        if values is None:
            value = self._on_value
        elif values in _OnOffAction.TRUE_VALUES:
            value = self._on_value
        elif values in _OnOffAction.FALSE_VALUES:
            value = self._off_value
        else:
            raise argparse.ArgumentTypeError(
                str(values) + ' is not a boolean value')

        super(_OnOffAction, self).__call__(
            parser, namespace, value, option_string=None)


class _EnableAction(_OnOffAction):
    """Action that defaults to False when absent and to True when parsed with
    the option to override the value by passing a bool-like value as an
    argument.
    """

    def __init__(self, **kwargs):
        kwargs['on_value'] = True
        kwargs['off_value'] = False
        super(_EnableAction, self).__init__(**kwargs)


class _DisableAction(_OnOffAction):
    """Action that defaults to True when absent and to False when parsed with
    the option to override the value by passing a bool-like value as an
    argument. When overridden the resulting value is negated, thus passing
    'True' will result in the destination being set to False.
    """

    def __init__(self, **kwargs):
        kwargs['on_value'] = False
        kwargs['off_value'] = True
        super(_DisableAction, self).__init__(**kwargs)


class _AppendAction(_MultiDestinationAction):
    """Action that append specified string to a list.
    """

    def __init__(self, join=None, separator=None, **kwargs):
        kwargs['nargs'] = None
        if kwargs.get('default', None) is None:
            if join is not None:
                kwargs['default'] = ''
            else:
                kwargs['default'] = []

        super(_AppendAction, self).__init__(**kwargs)

        self.join = join
        self.separator = separator

    def __call__(self, parser, namespace, values, option_string=None):
        if self.separator is not None:
            values = values.split(self.separator)
        else:
            values = [values]

        if self.join is not None:
            append = (self.join).join(values)
        else:
            append = list(values)

        for dest in self.dests():
            value = getattr(namespace, dest)
            if len(value) and self.join is not None:
                value = value + self.join

            value = value + append
            setattr(namespace, dest, value)


class _CompoundAction(argparse.Action):
    """Action containing multiple actions.
    """

    def __init__(self, actions, **kwargs):
        _actions = []
        for action in actions:
            _actions.append(action(**kwargs))

        kwargs.setdefault('nargs', _actions[0].nargs)
        kwargs.setdefault('metavar', _actions[0].metavar)
        kwargs.setdefault('choices', _actions[0].choices)

        super(_CompoundAction, self).__init__(**kwargs)
        self.actions = _actions
        # Compound actions should not infer a default dest.
        self.dest = argparse.SUPPRESS

    def __call__(self, *args):
        for action in self.actions:
            action(*args)


class _ActionPartial(object):
    """Partial to make option(flag, act) and option(flag, act()) equivalent.
    """

    def __init__(self, action):
        self.action = action

    def __call__(self, dests=None, **call_kwargs):
        def factory(*args, **kwargs):
            kwargs.update(call_kwargs)  # Innner kwargs are stronger
            if dests is None:
                return self.action(*args, **kwargs)
            return self.action(dests=dests, *args, **kwargs)

        return factory


# -----------------------------------------------------------------------------

class ArgumentParserBuilder(object):
    """Builder class exposing a rich DSL for constructing complex
    ArgumentParser instances.
    """

    def __init__(self, parser_class=None, **kwargs):
        if parser_class is None:
            parser_class = argparse.ArgumentParser

        assert issubclass(parser_class, argparse.ArgumentParser)

        self._parser = parser_class(**kwargs)
        self._current_group = self._parser
        self._defaults = {}

        self.set_action = _ActionPartial(_SetAction)
        self.set_int_action = _ActionPartial(_SetIntAction)
        self.set_float_action = _ActionPartial(_SetFloatAction)
        self.set_path_action = _ActionPartial(_SetPathAction)
        self.set_true_action = _ActionPartial(_SetTrueAction)
        self.set_false_action = _ActionPartial(_SetFalseAction)
        self.enable_action = _ActionPartial(_EnableAction)
        self.disable_action = _ActionPartial(_DisableAction)
        self.append_action = _ActionPartial(_AppendAction)
        self.unuspported_action = _ActionPartial(_UnsupportedAction)

    @classmethod
    def from_parser(parser):
        builder = ArgumentParserBuilder()
        builder._parser = parser
        return builder

    def build(self):
        self._parser.set_defaults(**self._defaults)
        return self._parser

    def add_option(self, names, *actions, **kwargs):
        if isinstance(names, str):
            names = [names]

        assert all(name.startswith('-') for name in names), (
            "add_option can't add positional arguments")

        # Unwrap partial actions.
        _actions = []
        for act in actions:
            if isinstance(act, _ActionPartial):
                act = act()
            _actions.append(act)
        actions = _actions

        if len(actions) == 0:
            # add_option(name) is equivalent to option(name, set_())
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

    def add_argument(self, dest, **kwargs):
        self._current_group.add_argument(dest, **kwargs)

    def set_defaults(self, *args, **kwargs):
        if len(args):
            assert len(args) == 2, 'Invalid set_default argument.'
            (names, default_value) = args
            if isinstance(names, str):
                names = [names]
            for name in names:
                kwargs[name] = default_value

        # Defaults will be added to the parser at build() time.
        self._defaults.update(kwargs)

    def in_group(self, description):
        self._current_group = self._parser.add_argument_group(description)

    def reset_group(self):
        self._current_group = self._parser

    @contextmanager
    def argument_group(self, description):
        old = self._current_group
        self._current_group = self._parser.add_argument_group(description)
        yield
        self._current_group = old

    @contextmanager
    def mutually_exclusive_group(self):
        old = self._current_group
        self._current_group = old.add_mutually_exclusive_group()
        yield
        self._current_group = old
