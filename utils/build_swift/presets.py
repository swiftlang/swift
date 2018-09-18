# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Swift preset parsing and handling functionality.
"""

from __future__ import absolute_import, unicode_literals

from collections import namedtuple
from contextlib import contextmanager

try:
    # Python 2
    import ConfigParser as configparser
    from StringIO import StringIO
except ImportError:
    import configparser
    from io import StringIO


__all__ = [
    'Error',
    'DuplicatePresetError',
    'DuplicateOptionError',
    'InterpolationError',
    'PresetNotFoundError',
    'UnparsedFilesError',

    'Preset',
    'PresetParser',
]


# -----------------------------------------------------------------------------

_PRESET_PREFIX = 'preset: '

_Mixin = namedtuple('_Mixin', ['name'])
_Argument = namedtuple('_Argument', ['name', 'value'])
_RawPreset = namedtuple('_RawPreset', ['name', 'options'])


def _interpolate_string(string, values):
    if string is None:
        return string

    return string % values


def _remove_prefix(string, prefix):
    if string.startswith(prefix):
        return string[len(prefix):]
    return string


@contextmanager
def _catch_duplicate_option_error():
    """Shim context object used for catching and rethrowing configparser's
    DuplicateOptionError, which was added in the Python 3 refactor.
    """

    if hasattr(configparser, 'DuplicateOptionError'):
        try:
            yield
        except configparser.DuplicateOptionError as e:
            preset_name = _remove_prefix(e.section, _PRESET_PREFIX)
            raise DuplicateOptionError(preset_name, e.option)

    else:
        yield


@contextmanager
def _catch_duplicate_section_error():
    """Shim context object used for catching and rethrowing configparser's
    DuplicateSectionError.
    """

    try:
        yield
    except configparser.DuplicateSectionError as e:
        preset_name = _remove_prefix(e.section, _PRESET_PREFIX)
        raise DuplicatePresetError(preset_name)


@contextmanager
def _convert_configparser_errors():
    with _catch_duplicate_option_error(), _catch_duplicate_section_error():
        yield


# -----------------------------------------------------------------------------
# Error classes

class Error(Exception):
    """Base class for preset errors.
    """

    def __init__(self, message=''):
        super(Error, self).__init__(self, message)

        self.message = message

    def __str__(self):
        return self.message

    __repr__ = __str__


class DuplicatePresetError(Error):
    """Raised when an existing preset would be overriden.
    """

    def __init__(self, preset_name):
        Error.__init__(self, '{} already exists'.format(preset_name))

        self.preset_name = preset_name


class DuplicateOptionError(Error):
    """Raised when an option is repeated in a single preset.
    """

    def __init__(self, preset_name, option):
        Error.__init__(self, '{} already exists in preset {}'.format(
            option, preset_name))

        self.preset_name = preset_name
        self.option = option


class InterpolationError(Error):
    """Raised when an error is encountered while interpolating use-provided
    values in preset arguments.
    """

    def __init__(self, preset_name, option, rawval, reference):
        Error.__init__(self, 'no value found for {} in "{}"'.format(
            reference, rawval))

        self.preset_name = preset_name
        self.option = option
        self.rawval = rawval
        self.reference = reference


class PresetNotFoundError(Error):
    """Raised when a requested preset cannot be found.
    """

    def __init__(self, preset_name):
        Error.__init__(self, '{} not found'.format(preset_name))

        self.preset_name = preset_name


class UnparsedFilesError(Error):
    """Raised when an error was encountered parsing one or more preset files.
    """

    def __init__(self, filenames):
        Error.__init__(self, 'unable to parse files: {}'.format(filenames))

        self.filenames = filenames


# -----------------------------------------------------------------------------

class Preset(namedtuple('Preset', ['name', 'args'])):
    """Container class used to wrap preset names and expanded argument lists.
    """

    # Keeps memory costs low according to the docs
    __slots__ = ()

    def format_args(self):
        """Format argument pairs for use in the command line.
        """

        args = []
        for (name, value) in self.args:
            if value is None:
                args.append(name)
            else:
                args.append('{}={}'.format(name, value))

        return args


class PresetParser(object):
    """Parser class used to read and manipulate Swift preset files.
    """

    def __init__(self):
        self._parser = configparser.RawConfigParser(allow_no_value=True)
        self._presets = {}

    def _parse_raw_preset(self, section):
        preset_name = _remove_prefix(section, _PRESET_PREFIX)

        try:
            section_items = self._parser.items(section)
        except configparser.InterpolationMissingOptionError as e:
            raise InterpolationError(preset_name, e.option, e.rawval,
                                     e.reference)

        args = []
        for (option, value) in section_items:
            # Ignore the '--' separator, it's no longer necessary
            if option == 'dash-dash':
                continue

            # Parse out mixin options
            if option == 'mixin-preset':
                lines = value.strip().splitlines()
                args += [_Mixin(option.strip()) for option in lines]
                continue

            option = '--' + option  # Format as a command-line option
            args.append(_Argument(option, value))

        return _RawPreset(preset_name, args)

    def _parse_raw_presets(self):
        for section in self._parser.sections():
            # Skip all non-preset sections
            if not section.startswith(_PRESET_PREFIX):
                continue

            raw_preset = self._parse_raw_preset(section)
            self._presets[raw_preset.name] = raw_preset

    def read(self, filenames):
        """Reads and parses preset files. Throws an UnparsedFilesError if any
        of the files couldn't be read.
        """

        with _convert_configparser_errors():
            parsed_files = self._parser.read(filenames)

        unparsed_files = set(filenames) - set(parsed_files)
        if len(unparsed_files) > 0:
            raise UnparsedFilesError(list(unparsed_files))

        self._parse_raw_presets()

    def read_file(self, file):
        """Reads and parses a single file.
        """

        self.read([file])

    def read_string(self, string):
        """Reads and parses a string containing preset definintions.
        """

        fp = StringIO(string)

        with _convert_configparser_errors():
            # ConfigParser changes drastically from Python 2 to 3
            if hasattr(self._parser, 'read_file'):
                self._parser.read_file(fp)
            else:
                self._parser.readfp(fp)

        self._parse_raw_presets()

    def _get_preset(self, name):
        preset = self._presets.get(name)
        if preset is None:
            raise PresetNotFoundError(name)

        if isinstance(preset, _RawPreset):
            preset = self._resolve_preset_mixins(preset)

            # Cache resolved preset
            self._presets[name] = preset

        return preset

    def _resolve_preset_mixins(self, raw_preset):
        """Resolve all mixins in a preset, fully expanding the arguments list.
        """

        assert isinstance(raw_preset, _RawPreset)

        # Expand mixin arguments
        args = []
        for option in raw_preset.options:
            if isinstance(option, _Mixin):
                args += self._get_preset(option.name).args
            elif isinstance(option, _Argument):
                args.append((option.name, option.value))
            else:
                # Should be unreachable
                raise ValueError('invalid argument type: {}', option.__class__)

        return Preset(raw_preset.name, args)

    def _interpolate_preset_vars(self, preset, vars):
        interpolated_args = []
        for (name, value) in preset.args:
            try:
                value = _interpolate_string(value, vars)
            except KeyError as e:
                raise InterpolationError(preset.name, name, value, e.args[0])

            interpolated_args.append((name, value))

        return Preset(preset.name, interpolated_args)

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

    def preset_names(self):
        """Returns a list of all parsed preset names.
        """

        return self._presets.keys()
