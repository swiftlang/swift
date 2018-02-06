# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


import os

from .utils import TestCase, UTILS_PATH
from ..presets import (InterpolationError, Preset, PresetNotFoundError,
                       PresetParser, UnparsedFilesError)

try:
    # Python 3
    import configparser
except ImportError:
    import ConfigParser as configparser


PRESET_FILES = [
    os.path.join(UTILS_PATH, 'build-presets.ini'),
]

PRESET_DEFAULTS = {
    'darwin_toolchain_alias': 'Alias',
    'darwin_toolchain_bundle_identifier': 'BundleIdentifier',
    'darwin_toolchain_display_name': 'DisplayName',
    'darwin_toolchain_display_name_short': 'DispalyNameShort',
    'darwin_toolchain_version': '1.0',
    'darwin_toolchain_xctoolchain_name': 'default',
    'extra_swift_args': '',
    'install_destdir': '/tmp/install',
    'install_symroot': '/tmp/install/symroot',
    'install_toolchain_dir': '/tmp/install/toolchain',
    'installable_package': '/tmp/install/pkg',
    'swift_install_destdir': '/tmp/install/swift',
    'symbols_package': '/path/to/symbols/package',
}

SAMPLE_PRESET = """
[preset: sample]

# This is a comment

ios
tvos
watchos
test
validation-test
lit-args=-v
compiler-vendor=apple

# The '--' argument is now unnecessary
dash-dash

verbose-build
build-ninja

# Default interpolation
install-symroot=%(install_symroot)s
"""

INVALID_PRESET = """
[preset: invalid]

ios
ios
"""

IGNORED_SECTION = """
[section_name]

random-options=1
"""

MIXIN_ORDER_PRESETS = """
[preset: test_mixin]
first-opt=0
second-opt=1


[preset: test]
first-opt=1
mixin-preset=test_mixin
second-opt=2
"""


# -----------------------------------------------------------------------------

class TestPreset(TestCase):

    def test_format_args(self):
        preset = Preset('sample', [('--ios', None), ('--test', '1')])

        self.assertEqual(preset.format_args(), ['--ios', '--test=1'])


# -----------------------------------------------------------------------------

class TestPresetParserMeta(type):
    """Metaclass used to dynamically generate test methods to validate all of
    the available presets.
    """

    def __new__(cls, name, bases, attrs):
        preset_parser = PresetParser()
        preset_parser.read(PRESET_FILES)

        # Generate tests for each preset
        for preset_name in preset_parser.preset_names():
            test_name = 'test_get_preset_' + preset_name
            attrs[test_name] = cls.generate_get_preset_test(
                preset_parser, preset_name)

        return super(TestPresetParserMeta, cls).__new__(
            cls, name, bases, attrs)

    @classmethod
    def generate_get_preset_test(cls, preset_parser, preset_name):
        def test(self):
            with self.assertNotRaises():
                preset_parser.get_preset(preset_name, vars=PRESET_DEFAULTS)

        return test


class TestPresetParser(TestCase):

    __metaclass__ = TestPresetParserMeta

    def test_read(self):
        parser = PresetParser()

        with self.assertNotRaises():
            parser.read(PRESET_FILES)

    def test_read_invalid_files(self):
        parser = PresetParser()

        with self.assertRaises(UnparsedFilesError):
            parser.read(['nonsense-presets.ini'])

    def test_read_file(self):
        parser = PresetParser()

        with self.assertNotRaises():
            parser.read_file(PRESET_FILES[0])

    def test_read_string(self):
        parser = PresetParser()

        with self.assertNotRaises():
            parser.read_string(SAMPLE_PRESET)

        preset = parser.get_preset('sample', vars={'install_symroot': '/tmp'})
        self.assertIsNotNone(preset)
        self.assertEqual(preset.name, 'sample')
        self.assertListEqual(preset.args, [
            (u'--ios', None),
            (u'--tvos', None),
            (u'--watchos', None),
            (u'--test', None),
            (u'--validation-test', None),
            (u'--lit-args', u'-v'),
            (u'--compiler-vendor', u'apple'),
            (u'--verbose-build', None),
            (u'--build-ninja', None),
            (u'--install-symroot', u'/tmp')
        ])

    def test_parser_ignores_non_preset_sections(self):
        parser = PresetParser()

        parser.read_string(IGNORED_SECTION)
        self.assertEqual(len(parser._presets), 0)

    def test_mixin_expansion_preserves_argument_order(self):
        """Mixins should be expanded in-place.
        """

        parser = PresetParser()

        parser.read_string(MIXIN_ORDER_PRESETS)

        preset = parser.get_preset('test')
        self.assertListEqual(preset.format_args(), [
            '--first-opt=1',

            # Mixin arguments
            '--first-opt=0',
            '--second-opt=1',

            '--second-opt=2',
        ])

    def test_duplicate_option_error(self):
        parser = PresetParser()

        # Skip this test if using the Python 2 ConfigParser module
        if not hasattr(configparser, 'DuplicateOptionError'):
            return

        with self.assertRaises(configparser.DuplicateOptionError):
            parser.read_string(INVALID_PRESET)

    def test_interpolation_error(self):
        parser = PresetParser()
        parser.read_string(SAMPLE_PRESET)

        with self.assertRaises(InterpolationError):
            parser.get_preset('sample')

    def test_get_missing_preset(self):
        parser = PresetParser()

        with self.assertRaises(PresetNotFoundError):
            parser.get_preset('preset')

    def test_preset_names(self):
        parser = PresetParser()

        parser.read_string(SAMPLE_PRESET)
        parser.read_string('[preset: test]\nios\n')

        self.assertListEqual(parser.preset_names(), ['sample', 'test'])
