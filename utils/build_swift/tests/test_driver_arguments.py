# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

from __future__ import print_function

import os
import sys
import unittest

from contextlib import contextmanager
from io import StringIO

from build_swift import driver_arguments
from build_swift.tests import expected_options

from swift_build_support.swift_build_support import migration
from swift_build_support.swift_build_support.SwiftBuildSupport import (
    get_all_preset_names,
    get_preset_options,
)

FILE_PATH = os.path.abspath(__file__)
TESTS_PATH = os.path.abspath(os.path.join(FILE_PATH, os.pardir))
BUILD_SWIFT_PATH = os.path.abspath(os.path.join(TESTS_PATH, os.pardir))
UTILS_PATH = os.path.abspath(os.path.join(BUILD_SWIFT_PATH, os.pardir))

BUILD_SCRIPT_IMPL = os.path.join(UTILS_PATH, 'build-script-impl')

PRESETS_FILES = [
    os.path.join(UTILS_PATH, 'build-presets.ini'),
]


class ParserError(Exception):
    pass


@contextmanager
def redirect_stderr(stream=None):
    stream = stream or StringIO()
    old_stderr, sys.stderr = sys.stderr, stream
    try:
        yield stream
    finally:
        sys.stderr = old_stderr


@contextmanager
def redirect_stdout(stream=None):
    stream = stream or StringIO()
    old_stdout, sys.stdout = sys.stdout, stream
    try:
        yield stream
    finally:
        sys.stdout = old_stdout


def _load_all_presets(presets_files):
    preset_names = get_all_preset_names(presets_files)

    # Hack to filter out mixins which are not expected to be valid presets
    preset_names = [n for n in preset_names if not n.startswith('mixin')]

    substitutions = {
        'install_destdir': '/tmp/install',
        'install_symroot': '/tmp/symroot',
        'installable_package': '/tmp/xcode-xyz-root.tar.gz',
    }

    presets = dict()
    for name in preset_names:
        try:
            # Attempt to parse preset
            presets[name] = get_preset_options(substitutions,
                                               presets_files, name)
        except SystemExit:
            continue

    return presets


class TestDriverArgumentParserMeta(type):
    """Metaclass used to dynamically generate test methods for each of the
    individual options accepted by the parser and methods to validate all of
    the presets.
    """

    def __new__(cls, name, bases, attrs):
        # Generate tests for each default value
        for dest, value in expected_options.EXPECTED_DEFAULTS.items():
            test_name = 'test_default_value_' + dest
            attrs[test_name] = cls.generate_default_value_test(dest, value)

        # Generate tests for each expected option
        for option in expected_options.EXPECTED_OPTIONS:
            test_name = 'test_option_' + option.sanitized_str()
            attrs[test_name] = cls.generate_option_test(option)

        # Generate tests for each preset
        presets = _load_all_presets(PRESETS_FILES)

        for name, args in presets.items():
            test_name = 'test_preset_' + name
            attrs[test_name] = cls.generate_preset_test(name, args)

        return super(TestDriverArgumentParserMeta, cls).__new__(
            cls, name, bases, attrs)

    @classmethod
    def generate_default_value_test(cls, dest, default_value):
        def test(self):
            with self.assertNotRaises(ParserError):
                parsed_values = self.parse_args([])

            parsed_value = getattr(parsed_values, dest)
            if default_value.__class__ is str:
                parsed_value = str(parsed_value)

            self.assertEqual(default_value, parsed_value,
                             'Invalid default value for "{}": {} != {}'
                             .format(dest, default_value, parsed_value))

        return test

    @classmethod
    def _generate_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                args = self.parse_args([option.option_string])
                self.assertEqual(getattr(args, option.dest), option.value)

            with self.assertRaises(ParserError):
                self.parse_args([option.option_string, 'foo'])

        return test

    @classmethod
    def _generate_append_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                # Range size is arbitrary, just needs to be more than once
                for i in range(1, 4):
                    args = self.parse_args([option.option_string, 'ARG'] * i)
                    self.assertEqual(getattr(args, option.dest), ['ARG'] * i)

        return test

    @classmethod
    def _generate_choices_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                for choice in option.choices:
                    args = self.parse_args([option.option_string, str(choice)])
                    self.assertEqual(getattr(args, option.dest), choice)

            with self.assertRaises(ParserError):
                self.parse_args([option.option_string, 'INVALID'])

        return test

    @classmethod
    def _generate_help_option_test(cls, option):
        def test(self):
            with redirect_stdout() as output, self.assertRaises(ParserError):
                self.parse_args([option.option_string])
                self.assertNotEmpty(output)

        return test

    @classmethod
    def _generate_int_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                for i in [0, 1, 42]:
                    args = self.parse_args([option.option_string, str(i)])
                    self.assertEqual(int(getattr(args, option.dest)), i)

            # FIXME: int-type options should not accept non-int strings
            # with self.assertRaises(ParserError):
            #     self.parse_args([option.option_string, str(0.0)])
            #     self.parse_args([option.option_string, str(1.0)])
            #     self.parse_args([option.option_string, str(3.14)])
            #     self.parse_args([option.option_string, 'NaN'])

        return test

    @classmethod
    def _generate_path_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                self.parse_args([option.option_string, sys.executable])

            # FIXME: path-type options should not accept non-path inputs
            # with self.assertRaises(ParserError):
            #     self.parse_args([option.option_string, 'foo'])

        return test

    @classmethod
    def _generate_str_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                self.parse_args([option.option_string, 'foo'])

        return test

    @classmethod
    def _generate_toggle_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                # Standalone argument
                self.parse_args([option.option_string])

                # True values
                # self.parse_args([option.option_string, True])
                # self.parse_args([option.option_string, 1])
                self.parse_args([option.option_string, '1'])
                self.parse_args([option.option_string, 'true'])
                self.parse_args([option.option_string, 'True'])
                # self.parse_args([option.option_string, 'TRUE'])

                # False values
                # self.parse_args([option.option_string, False])
                # self.parse_args([option.option_string, 0])
                self.parse_args([option.option_string, '0'])
                self.parse_args([option.option_string, 'false'])
                self.parse_args([option.option_string, 'False'])
                # self.parse_args([option.option_string, 'FALSE'])

        return test

    @classmethod
    def _generate_unsupported_option_test(cls, option):
        def test(self):
            with self.assertRaises(ParserError):
                self.parse_args([option.option_string])

        return test

    @classmethod
    def generate_option_test(cls, option):
        if option.__class__ is expected_options.Option:
            return cls._generate_option_test(option)
        elif option.__class__ is expected_options.AppendOption:
            return cls._generate_append_option_test(option)
        elif option.__class__ is expected_options.ChoicesOption:
            return cls._generate_choices_option_test(option)
        elif option.__class__ is expected_options.HelpOption:
            return cls._generate_help_option_test(option)
        elif option.__class__ is expected_options.IntOption:
            return cls._generate_int_option_test(option)
        elif option.__class__ is expected_options.PathOption:
            return cls._generate_path_option_test(option)
        elif option.__class__ is expected_options.StrOption:
            return cls._generate_str_option_test(option)
        elif option.__class__ is expected_options.ToggleOption:
            return cls._generate_toggle_option_test(option)
        elif option.__class__ is expected_options.UnsupportedOption:
            return cls._generate_unsupported_option_test(option)

        # Ignore all IgnoreOption tests since they should be manually tested
        elif option.__class__ is expected_options.IgnoreOption:
            return lambda self: None

        # Catch-all meaningless test
        return lambda self: \
            self.fail('Unexpected option "{}"'.format(option.option_string))

    @classmethod
    def generate_preset_test(cls, preset_name, preset_args):
        def test(self):
            try:
                self.parse_args(preset_args, check_impl_args=True)
            except ParserError as e:
                self.fail('Failed to parse preset "{}": {}'.format(
                    preset_name, e))

        return test


class TestDriverArgumentParser(unittest.TestCase):

    __metaclass__ = TestDriverArgumentParserMeta

    def parse_args(self, args, error_message=None, check_impl_args=False):
        if error_message is None:
            error_message = 'failed to parse arguments: ' + str(args)

        with open(os.devnull, 'w') as devnull:
            try:
                with redirect_stderr(devnull), redirect_stdout(devnull):
                    namespace = migration.parse_args(self.parser, args)
            except (SystemExit, ValueError) as e:
                raise ParserError(error_message, e)

            if not namespace.build_script_impl_args and not check_impl_args:
                return namespace

            try:
                with redirect_stderr(devnull), redirect_stdout(devnull):
                    migration.check_impl_args(BUILD_SCRIPT_IMPL,
                                              namespace.build_script_impl_args)
            except (SystemExit, ValueError) as e:
                raise ParserError(error_message, e)

        return namespace

    @contextmanager
    def assertNotRaises(self, exception):
        assert issubclass(exception, BaseException)

        try:
            yield
        except exception as e:
            self.fail(str(e))

    def setUp(self):
        self.parser = driver_arguments.create_argument_parser()

    # -------------------------------------------------------------------------
    # Manual option tests

    def test_option_clang_compiler_version(self):
        option_string = '--clang-compiler-version'

        with self.assertNotRaises(ParserError):
            self.parse_args([option_string, '5.0.0'])
            self.parse_args([option_string, '5.0.1'])
            self.parse_args([option_string, '5.0.0.1'])

        with self.assertRaises(ParserError):
            self.parse_args([option_string, '1'])
            self.parse_args([option_string, '1.2'])
            self.parse_args([option_string, '0.0.0.0.1'])

    def test_option_clang_user_visible_version(self):
        option_string = '--clang-user-visible-version'

        with self.assertNotRaises(ParserError):
            self.parse_args([option_string, '5.0.0'])
            self.parse_args([option_string, '5.0.1'])
            self.parse_args([option_string, '5.0.0.1'])

        with self.assertRaises(ParserError):
            self.parse_args([option_string, '1'])
            self.parse_args([option_string, '1.2'])
            self.parse_args([option_string, '0.0.0.0.1'])

    def test_option_swift_compiler_version(self):
        option_string = '--swift-compiler-version'

        with self.assertNotRaises(ParserError):
            self.parse_args([option_string, '4.1'])
            self.parse_args([option_string, '4.0.1'])
            self.parse_args([option_string, '200.99.1'])

        with self.assertRaises(ParserError):
            self.parse_args([option_string, '1'])
            self.parse_args([option_string, '0.0.0.1'])

    def test_option_swift_user_visible_version(self):
        option_string = '--swift-user-visible-version'

        with self.assertNotRaises(ParserError):
            self.parse_args([option_string, '4.1'])
            self.parse_args([option_string, '4.0.1'])
            self.parse_args([option_string, '200.99.1'])

        with self.assertRaises(ParserError):
            self.parse_args([option_string, '1'])
            self.parse_args([option_string, '0.0.0.1'])

    # -------------------------------------------------------------------------
    # Implied defaults tests

    def test_implied_defaults_assertions(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--assertions'])

            self.assertTrue(args.cmark_assertions)
            self.assertTrue(args.llvm_assertions)
            self.assertTrue(args.swift_assertions)
            self.assertTrue(args.swift_stdlib_assertions)

    def test_implied_defaults_cmark_build_variant(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--debug-cmark'])
            self.assertTrue(args.build_cmark)

    def test_implied_defaults_lldb_build_variant(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--debug-lldb'])
            self.assertTrue(args.build_lldb)

        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--lldb-assertions'])
            self.assertTrue(args.build_lldb)

    def test_implied_defaults_build_variant(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--debug'])

            self.assertEquals(args.cmark_build_variant, 'Debug')
            self.assertEquals(args.foundation_build_variant, 'Debug')
            self.assertEquals(args.libdispatch_build_variant, 'Debug')
            self.assertEquals(args.libicu_build_variant, 'Debug')
            self.assertEquals(args.lldb_build_variant, 'Debug')
            self.assertEquals(args.llvm_build_variant, 'Debug')
            self.assertEquals(args.swift_build_variant, 'Debug')
            self.assertEquals(args.swift_stdlib_build_variant, 'Debug')

    def test_implied_defaults_skip_build(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--skip-build'])

            self.assertFalse(args.build_benchmarks)

            self.assertFalse(args.build_linux)
            self.assertFalse(args.build_android)
            self.assertFalse(args.build_freebsd)
            self.assertFalse(args.build_cygwin)
            self.assertFalse(args.build_osx)
            self.assertFalse(args.build_ios)
            self.assertFalse(args.build_tvos)
            self.assertFalse(args.build_watchos)

            self.assertFalse(args.build_foundation)
            self.assertFalse(args.build_libdispatch)
            self.assertFalse(args.build_libicu)
            self.assertFalse(args.build_lldb)
            self.assertFalse(args.build_llbuild)
            self.assertFalse(args.build_playgroundlogger)
            self.assertFalse(args.build_playgroundsupport)
            self.assertFalse(args.build_swiftpm)
            self.assertFalse(args.build_xctest)

    def test_implied_defaults_skip_build_ios(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--skip-build-ios'])
            self.assertFalse(args.build_ios_device)
            self.assertFalse(args.build_ios_simulator)

            # Also implies that the tests should be skipped
            self.assertTrue(args.skip_test_ios_host)
            self.assertTrue(args.skip_test_ios_simulator)

    def test_implied_defaults_skip_build_tvos(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--skip-build-tvos'])
            self.assertFalse(args.build_tvos_device)
            self.assertFalse(args.build_tvos_simulator)

            # Also implies that the tests should be skipped
            self.assertTrue(args.skip_test_tvos_host)
            self.assertTrue(args.skip_test_tvos_simulator)

    def test_implied_defaults_skip_build_watchos(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--skip-build-watchos'])
            self.assertFalse(args.build_watchos_device)
            self.assertFalse(args.build_watchos_simulator)

            # Also implies that the tests should be skipped
            self.assertTrue(args.skip_test_watchos_host)
            self.assertTrue(args.skip_test_watchos_simulator)

    def test_implied_defaults_validation_test(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--validation-test'])
            self.assertTrue(args.test)

    def test_implied_defaults_test_optimized(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--test-optimized'])
            self.assertTrue(args.test)

    def test_implied_defaults_test_optimize_for_size(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--test-optimize-for-size'])
            self.assertTrue(args.test)

    def test_implied_defaults_skip_all_tests(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args([
                '--test', '0',
                '--validation-test', '0',
                '--long-test', '0',
            ])

            self.assertTrue(args.skip_test_linux)
            self.assertTrue(args.skip_test_freebsd)
            self.assertTrue(args.skip_test_cygwin)
            self.assertTrue(args.skip_test_osx)
            self.assertTrue(args.skip_test_ios)
            self.assertTrue(args.skip_test_tvos)
            self.assertTrue(args.skip_test_watchos)

    def test_implied_defaults_skip_test_ios(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--skip-test-ios'])
            self.assertTrue(args.skip_test_ios_host)
            self.assertTrue(args.skip_test_ios_simulator)

    def test_implied_defaults_skip_test_tvos(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--skip-test-tvos'])
            self.assertTrue(args.skip_test_tvos_host)
            self.assertTrue(args.skip_test_tvos_simulator)

    def test_implied_defaults_skip_test_watchos(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--skip-test-watchos'])
            self.assertTrue(args.skip_test_watchos_host)
            self.assertTrue(args.skip_test_watchos_simulator)

    def test_implied_defaults_skip_build_android(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--android', '0'])
            self.assertTrue(args.skip_test_android_host)

        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--skip-build-android'])
            self.assertTrue(args.skip_test_android_host)

    def test_implied_defaults_host_test(self):
        with self.assertNotRaises(ParserError):
            args = self.parse_args(['--host-test', '0'])
            self.assertTrue(args.skip_test_ios_host)
            self.assertTrue(args.skip_test_tvos_host)
            self.assertTrue(args.skip_test_watchos_host)
            self.assertTrue(args.skip_test_android_host)


if __name__ == '__main__':
    unittest.main()
