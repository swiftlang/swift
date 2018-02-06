# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Swift preset parsing and handling functionality.
"""


from collections import namedtuple

try:
    # Python 3
    import configparser
    from io import StringIO
except ImportError:
    import ConfigParser as configparser
    from StringIO import StringIO


__all__ = [
    'Preset',
    'PresetParser',

    'InterpolationError',
    'MissingOptionError',
    'PresetNotFoundError',
    'UnparsedFilesError',
]


# -----------------------------------------------------------------------------

_Mixin = namedtuple('_Mixin', ['name'])
_Argument = namedtuple('_Argument', ['name', 'value'])
_RawPreset = namedtuple('_RawPreset', ['name', 'options'])


def _interpolate_string(string, values):
    if string is None:
        return string

    try:
        return string % values
    except KeyError as e:
        raise InterpolationError(e.message)


def _remove_prefix(string, prefix):
    if string.startswith(prefix):
        return string[len(prefix):]
    return string


# -----------------------------------------------------------------------------

class InterpolationError(Exception):
    """Error indicating a filaure while interpolating variables in preset
    argument values.
    """

    pass


class MissingOptionError(Exception):
    """Error indicating a missing option while parsing presets.
    """

    pass


class PresetNotFoundError(Exception):
    """Error indicating failure when attempting to get a preset.
    """

    pass


class UnparsedFilesError(Exception):
    """Error indicating failure when parsing preset files.
    """

    pass


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


# -----------------------------------------------------------------------------

class PresetParser(object):
    """Parser class used to read and manipulate Swift preset files.
    """

    _PRESET_PREFIX = 'preset: '

    def __init__(self):
        self._parser = configparser.RawConfigParser(allow_no_value=True)
        self._presets = {}

    def _parse_raw_preset(self, section):
        preset_name = _remove_prefix(section, PresetParser._PRESET_PREFIX)

        try:
            section_items = self._parser.items(section)
        except configparser.InterpolationMissingOptionError as e:
            raise MissingOptionError(e)

        args = []
        for (name, value) in section_items:
            # Ignore the '--' separator, it's no longer necessary
            if name == 'dash-dash':
                continue

            # Parse out mixin names
            if name == 'mixin-preset':
                lines = value.strip().splitlines()
                args += [_Mixin(name.strip()) for name in lines]
                continue

            name = '--' + name  # Format as an option name
            args.append(_Argument(name, value))

        return _RawPreset(preset_name, args)

    def _parse_raw_presets(self):
        for section in self._parser.sections():
            # Skip all non-preset sections
            if not section.startswith(PresetParser._PRESET_PREFIX):
                continue

            raw_preset = self._parse_raw_preset(section)
            self._presets[raw_preset.name] = raw_preset

    def read(self, filenames):
        """Reads and parses preset files. Throws an UnparsedFilesError if any
        of the files couldn't be read.
        """

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

        fp = StringIO(unicode(string))

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
                raise ValueError('invalid argument type: {}', option.__class__)

        return Preset(raw_preset.name, args)

    def _interpolate_preset_vars(self, preset, vars):
        interpolated_args = []
        for (name, value) in preset.args:
            value = _interpolate_string(value, vars)
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
