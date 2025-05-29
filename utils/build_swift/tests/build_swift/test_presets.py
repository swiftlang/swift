# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


import configparser
import os
import unittest

from build_swift import constants
from build_swift import presets
from build_swift.presets import Preset, PresetParser

from .. import utils


# -----------------------------------------------------------------------------
# Constants

PRESET_FILES = [
    os.path.join(constants.UTILS_PATH, 'build-presets.ini'),
]

PRESET_DEFAULTS = {
    'darwin_toolchain_alias': 'Alias',
    'darwin_toolchain_bundle_identifier': 'BundleIdentifier',
    'darwin_toolchain_display_name': 'DisplayName',
    'darwin_toolchain_display_name_short': 'DisplayNameShort',
    'darwin_toolchain_version': '1.0',
    'darwin_toolchain_xctoolchain_name': 'default',
    'extra_swift_args': '',
    'install_destdir': '/tmp/install',
    'install_symroot': '/tmp/install/symroot',
    'install_all': False,
    'install_toolchain_dir': '/tmp/install/toolchain',
    'install_prefix': '/usr',
    'installable_package': '/tmp/install/pkg',
    'swift_install_destdir': '/tmp/install/swift',
    'symbols_package': '/path/to/symbols/package',
    'ndk_path': '/path/to/ndk',
    'arm_dir': '/path/to/arm',
    'toolchain_path': '/tmp/toolchain',
    'build_subdir': 'test_build_subdir',
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

INTERPOLATED_PRESET = """
[preset: test]

install-symroot=%(install_symroot)s
"""

DUPLICATE_PRESET_NAMES = """
[preset: test]
ios


[preset: test]
tvos
"""

DUPLICATE_PRESET_OPTIONS = """
[preset: test]

ios
ios
"""

EXPAND_OPTION_NAME = """
[preset: test]

%(my_option)s
"""


# -----------------------------------------------------------------------------

class TestPreset(unittest.TestCase):

    def test_args(self):
        preset = Preset('sample', [('ios', None), ('test', '1')])

        self.assertEqual(preset.args, ['--ios', '--test=1'])


# -----------------------------------------------------------------------------

class TestPresetParserMeta(type):
    """Metaclass used to dynamically generate test methods to validate all of
    the available presets.
    """

    def __new__(cls, name, bases, attrs):
        preset_parser = PresetParser()
        preset_parser.read_files(PRESET_FILES)

        # Generate tests for each preset
        for preset_name in preset_parser.preset_names:
            test_name = 'test_get_preset_' + preset_name
            attrs[test_name] = cls.generate_get_preset_test(
                preset_parser, preset_name)

        return super(TestPresetParserMeta, cls).__new__(
            cls, name, bases, attrs)

    @classmethod
    def generate_get_preset_test(cls, preset_parser, preset_name):
        def test(self):
            preset_parser.get_preset(preset_name, vars=PRESET_DEFAULTS)

        return test


class TestPresetParser(unittest.TestCase, metaclass=TestPresetParserMeta):

    def test_read_files(self):
        parser = PresetParser()
        parser.read_files(PRESET_FILES)

    def test_read_invalid_files(self):
        parser = PresetParser()

        with self.assertRaises(presets.UnparsedFilesError) as cm:
            parser.read_files(['nonsense-presets.ini'])

        e = cm.exception
        unparsed = e.unparsed_files
        self.assertEqual(len(unparsed), 1)
        self.assertEqual(unparsed[0].filename, 'nonsense-presets.ini')
        self.assertIsInstance(unparsed[0].reason, IOError)

    def test_read_file(self):
        parser = PresetParser()
        parser.read_file(PRESET_FILES[0])

    def test_read_string(self):
        parser = PresetParser()
        parser.read_string(SAMPLE_PRESET)

        preset = parser.get_preset('sample', vars={'install_symroot': '/tmp'})
        self.assertIsNotNone(preset)
        self.assertEqual(preset.name, 'sample')
        self.assertListEqual(preset.options, [
            ('ios', None),
            ('tvos', None),
            ('watchos', None),
            ('test', None),
            ('validation-test', None),
            ('lit-args', '-v'),
            ('compiler-vendor', 'apple'),
            ('verbose-build', None),
            ('build-ninja', None),
            ('install-symroot', '/tmp')
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
        self.assertListEqual(preset.args, [
            '--first-opt=1',

            # Mixin arguments
            '--first-opt=0',
            '--second-opt=1',

            '--second-opt=2',
        ])

    def test_interpolation_error(self):
        parser = PresetParser()
        parser.read_string(INTERPOLATED_PRESET)

        with self.assertRaises(presets.InterpolationError) as cm:
            parser.get_preset('test')

        e = cm.exception
        self.assertEqual(e.preset_name, 'test')
        self.assertEqual(e.option, 'install-symroot')
        self.assertEqual(e.rawval, '%(install_symroot)s')
        self.assertEqual(e.reference, 'install_symroot')

    @utils.requires_attr(configparser, 'DuplicateOptionError')
    def test_duplicate_option_error(self):
        parser = PresetParser()

        with self.assertRaises(presets.DuplicateOptionError) as cm:
            parser.read_string(DUPLICATE_PRESET_OPTIONS)

        e = cm.exception
        self.assertEqual(e.preset_name, 'test')
        self.assertEqual(e.option, 'ios')

    @utils.requires_attr(configparser, 'DuplicateOptionError')
    def test_duplicate_preset_error(self):
        parser = PresetParser()

        with self.assertRaises(presets.DuplicatePresetError) as cm:
            parser.read_string(DUPLICATE_PRESET_NAMES)

        e = cm.exception
        self.assertEqual(e.preset_name, 'test')

    def test_get_preset_raw(self):
        parser = PresetParser()
        parser.read_string(INTERPOLATED_PRESET)

        preset = parser.get_preset('test', raw=True)
        self.assertEqual(preset.options, [
            ('install-symroot', '%(install_symroot)s')
        ])

    def test_get_missing_preset(self):
        parser = PresetParser()

        with self.assertRaises(presets.PresetNotFoundError) as cm:
            parser.get_preset('test')

        e = cm.exception
        self.assertEqual(e.preset_name, 'test')

    def test_preset_names(self):
        parser = PresetParser()

        parser.read_string('[preset: foo]')
        parser.read_string('[preset: bar]')
        parser.read_string('[preset: baz]')

        self.assertEqual(set(parser.preset_names),
                         set(['foo', 'bar', 'baz']))

    def test_expand_option_name(self):
        parser = PresetParser()
        parser.read_string(EXPAND_OPTION_NAME)

        preset = parser.get_preset('test', vars={'my_option': 'macos'})
        self.assertEqual(preset.options, [
            ('macos', None),
        ])
