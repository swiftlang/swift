# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Swift preset parsing and handling functionality.
"""


from __future__ import absolute_import, unicode_literals

import functools
import io
from collections import OrderedDict, namedtuple

from six import StringIO
from six.moves import configparser

from . import class_utils


__all__ = [
    'PresetError',
    'DuplicatePresetError',
    'DuplicateOptionError',
    'InterpolationError',
    'PresetNotFoundError',
    'UnparsedFilesError',

    'Preset',
    'PresetParser',
]


# -----------------------------------------------------------------------------
# Constants

_PRESET_PREFIX = 'preset: '


# -----------------------------------------------------------------------------
# Helpers

_Mixin = namedtuple('_Mixin', ['name'])
_Option = namedtuple('_Option', ['name', 'value'])
_RawPreset = namedtuple('_RawPreset', ['name', 'options'])

_UnparsedFile = namedtuple('_UnparsedFile', ['filename', 'reason'])


def _interpolate_string(string, values):
    if string is None:
        return string

    return string % values


def _remove_prefix(string, prefix):
    if string.startswith(prefix):
        return string[len(prefix):]
    return string


def _catch_duplicate_option_error(func):
    """Decorator used to catch and rethrowing configparser's
    DuplicateOptionError.
    """

    if not hasattr(configparser, 'DuplicateOptionError'):
        return func

    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except configparser.DuplicateOptionError as e:
            preset_name = _remove_prefix(e.section, _PRESET_PREFIX).strip()
            raise DuplicateOptionError(preset_name, e.option)

    return wrapper


def _catch_duplicate_section_error(func):
    """Decorator used to catch and rethrowing configparser's
    DuplicateSectionError.
    """

    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except configparser.DuplicateSectionError as e:
            preset_name = _remove_prefix(e.section, _PRESET_PREFIX).strip()
            raise DuplicatePresetError(preset_name)

    return wrapper


# -----------------------------------------------------------------------------
# Errors

class PresetError(Exception):
    """Base class for preset errors.
    """

    def __init__(self, message=''):
        super(PresetError, self).__init__(self, message)

        self.message = message

    def __str__(self):
        return self.message

    __repr__ = __str__


class DuplicatePresetError(PresetError):
    """Raised when an existing preset would be overriden.
    """

    def __init__(self, preset_name):
        super(DuplicatePresetError, self).__init__(
            '{} already exists'.format(preset_name))

        self.preset_name = preset_name


class DuplicateOptionError(PresetError):
    """Raised when an option is repeated in a single preset.
    """

    def __init__(self, preset_name, option):
        super(DuplicateOptionError, self).__init__(
            '{} already exists in preset {}'.format(option, preset_name))

        self.preset_name = preset_name
        self.option = option


class InterpolationError(PresetError):
    """Raised when an error is encountered while interpolating use-provided
    values in preset arguments.
    """

    def __init__(self, preset_name, option, rawval, reference):
        super(InterpolationError, self).__init__(
            'no value found for {} in "{}"'.format(reference, rawval))

        self.preset_name = preset_name
        self.option = option
        self.rawval = rawval
        self.reference = reference


class PresetNotFoundError(PresetError):
    """Raised when a requested preset cannot be found.
    """

    def __init__(self, preset_name):
        super(PresetNotFoundError, self).__init__(
            '{} not found'.format(preset_name))

        self.preset_name = preset_name


class UnparsedFilesError(PresetError):
    """Raised when an error was encountered parsing one or more preset files.
    """

    def __init__(self, unparsed_files):
        super(UnparsedFilesError, self).__init__(
            'unable to parse files: {}'.format(unparsed_files))

        self.unparsed_files = unparsed_files


# -----------------------------------------------------------------------------

@class_utils.generate_repr('name', 'options')
class Preset(object):
    """Container class used to wrap preset names and expanded options list.
    """

    __slots__ = ('name', 'options')

    def __init__(self, name, options):
        self.name = name
        self.options = options

    def __str__(self):
        return repr(self)

    @property
    def args(self):
        """Format options into command line arguments.
        """

        args = []
        for (name, value) in self.options:
            if value is None:
                args.append('--{}'.format(name))
            else:
                args.append('--{}={}'.format(name, value))

        return args


# -----------------------------------------------------------------------------
# Preset Parsing

class PresetParser(object):
    """Parser class used to read and manipulate Swift preset files.
    """

    def __init__(self):
        self._parser = configparser.RawConfigParser(allow_no_value=True)
        self._presets = OrderedDict()

    # -------------------------------------------------------------------------
    # Properties

    @property
    def preset_names(self):
        """Returns a list of all parsed preset names in the order they were
        parsed.
        """

        return self._presets.keys()

    @property
    def presets(self):
        """Returns a list of all parsed presets in the order they were parsed.
        """

        return self._presets.values()

    # -------------------------------------------------------------------------
    # Parsing

    def _parse_raw_preset(self, section):
        preset_name = _remove_prefix(section, _PRESET_PREFIX)

        try:
            section_items = self._parser.items(section)
        except configparser.InterpolationMissingOptionError as e:
            raise InterpolationError(
                preset_name, e.option, e.rawval, e.reference)

        options = []
        for (name, value) in section_items:
            # Ignore the '--' separator, it's no longer necessary
            if name == 'dash-dash':
                continue

            # Parse out mixin options
            if name == 'mixin-preset':
                lines = value.strip().splitlines()
                options += [_Mixin(mixin_name.strip()) for mixin_name in lines]
                continue

            options.append(_Option(name, value))

        return _RawPreset(preset_name, options)

    def _parse_raw_presets(self):
        for section in self._parser.sections():
            # Skip all non-preset sections
            if not section.startswith(_PRESET_PREFIX):
                continue

            raw_preset = self._parse_raw_preset(section)
            self._presets[raw_preset.name] = raw_preset

    @_catch_duplicate_option_error
    @_catch_duplicate_section_error
    def read_file(self, filename):
        """Reads and parses a single file.
        """

        with io.open(filename, 'r') as fp:
            if hasattr(self._parser, 'read_file'):
                self._parser.read_file(fp)
            else:
                self._parser.readfp(fp)

        self._parse_raw_presets()

    def read_files(self, filenames):
        """Reads and parses preset files. Throws an UnparsedFilesError if any
        of the files couldn't be read.
        """

        unparsed_files = []
        for filename in filenames:
            try:
                self.read_file(filename)
            except Exception as e:
                unparsed_files.append(_UnparsedFile(filename, e))

        if len(unparsed_files) > 0:
            raise UnparsedFilesError(unparsed_files)

        self._parse_raw_presets()

    @_catch_duplicate_option_error
    @_catch_duplicate_section_error
    def read_string(self, string):
        """Reads and parses a string containing preset definintions.
        """

        fp = StringIO(string)

        # ConfigParser changes drastically from Python 2 to 3
        if hasattr(self._parser, 'read_file'):
            self._parser.read_file(fp)
        else:
            self._parser.readfp(fp)

        self._parse_raw_presets()

    # -------------------------------------------------------------------------
    # Resolving

    def _resolve_preset_mixins(self, raw_preset):
        """Resolve all mixins in a preset, fully expanding the options list.
        """

        assert isinstance(raw_preset, _RawPreset)

        # Expand mixin options.
        options = []
        for option in raw_preset.options:
            if isinstance(option, _Mixin):
                options += self._get_preset(option.name).options
            elif isinstance(option, _Option):
                options.append((option.name, option.value))
            else:
                # Should be unreachable.
                raise ValueError('invalid argument type: {}', option.__class__)

        return Preset(raw_preset.name, options)

    def _get_preset(self, name):
        preset = self._presets.get(name)
        if preset is None:
            raise PresetNotFoundError(name)

        if isinstance(preset, _RawPreset):
            preset = self._resolve_preset_mixins(preset)

            # Cache resolved preset
            self._presets[name] = preset

        return preset

    def _interpolate_preset_vars(self, preset, vars):
        interpolated_options = []
        for (name, value) in preset.options:
            try:
                value = _interpolate_string(value, vars)
            except KeyError as e:
                raise InterpolationError(
                    preset.name, name, value, e.args[0])

            interpolated_options.append((name, value))

        return Preset(preset.name, interpolated_options)

    def get_preset(self, name, raw=False, vars=None):
        """Returns the preset with the requested name or throws a
        PresetNotFoundError.

        If raw is False vars will be interpolated into the preset arguments.
        Otherwise presets will be returned without interpolation.

        Presets are retrieved using a dynamic caching algorithm that expands
        only the requested preset and it's mixins recursively. Every expanded
        preset is then cached. All subsequent expansions or calls to
        `get_preset` for any pre-expanded presets will use the cached results.
        """

        vars = vars or {}

        preset = self._get_preset(name)
        if not raw:
            preset = self._interpolate_preset_vars(preset, vars)

        return preset
