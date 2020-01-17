# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


import os
import platform
import sys
import unittest
from contextlib import contextmanager
from io import StringIO

from swift_build_support.swift_build_support import migration
from swift_build_support.swift_build_support.SwiftBuildSupport import (
    get_all_preset_names,
    get_preset_options,
)

from . import expected_options as eo
from .. import argparse
from .. import driver_arguments


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
        for dest, value in eo.EXPECTED_DEFAULTS.items():
            test_name = 'test_default_value_' + dest
            attrs[test_name] = cls.generate_default_value_test(dest, value)

        # Generate tests for each expected option
        for option in eo.EXPECTED_OPTIONS:
            test_name = 'test_option_' + option.sanitized_string()
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
                parsed_values = self.parse_default_args([])

            parsed_value = getattr(parsed_values, dest)
            if default_value.__class__ is str:
                parsed_value = str(parsed_value)

            self.assertEqual(default_value, parsed_value,
                             'Invalid default value for "{}": {} != {}'
                             .format(dest, default_value, parsed_value))

        return test

    @classmethod
    def _generate_help_option_test(cls, option):
        def test(self):
            with redirect_stdout() as output, self.assertRaises(ParserError):
                self.parse_args([option.option_string])
                self.assertNotEmpty(output)

        return test

    @classmethod
    def _generate_set_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                namespace = self.parse_args([option.option_string])
                self.assertEqual(getattr(namespace, option.dest), option.value)

            with self.assertRaises(ParserError):
                self.parse_args([option.option_string, 'foo'])

        return test

    @classmethod
    def _generate_set_true_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                # TODO: Move to unit-tests for the action class
                namespace = self.parse_args([])
                self.assertFalse(getattr(namespace, option.dest))

                namespace = self.parse_args([option.option_string])
                self.assertTrue(getattr(namespace, option.dest))

        return test

    @classmethod
    def _generate_set_false_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                # TODO: Move to unit-tests for the action class
                namespace = self.parse_args([])
                self.assertTrue(getattr(namespace, option.dest))

                namespace = self.parse_args([option.option_string])
                self.assertFalse(getattr(namespace, option.dest))

        return test

    @classmethod
    def _generate_enable_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                # TODO: Move to unit-tests for the action class
                # Test parsing True values
                self.parse_args([option.option_string, '1'])
                self.parse_args([option.option_string, 'true'])
                self.parse_args([option.option_string, 'True'])
                self.parse_args([option.option_string, 'TRUE'])

                # TODO: Move to unit-tests for the action class
                # Test parsing False values
                self.parse_args([option.option_string, '0'])
                self.parse_args([option.option_string, 'false'])
                self.parse_args([option.option_string, 'False'])
                self.parse_args([option.option_string, 'FALSE'])

                # TODO: Move to unit-tests for the action class
                # Test default value
                namespace = self.parse_args([option.option_string])
                self.assertTrue(getattr(namespace, option.dest))

                # Test setting value to True
                namespace = self.parse_args([option.option_string, 'True'])
                self.assertTrue(getattr(namespace, option.dest))

                # Test setting value to False
                namespace = self.parse_args([option.option_string, 'False'])
                self.assertFalse(getattr(namespace, option.dest))

        return test

    @classmethod
    def _generate_disable_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                # TODO: Move to unit-tests for the action class
                # Test parsing True values
                self.parse_args([option.option_string, '1'])
                self.parse_args([option.option_string, 'true'])
                self.parse_args([option.option_string, 'True'])
                self.parse_args([option.option_string, 'TRUE'])

                # TODO: Move to unit-tests for the action class
                # Test parsing False values
                self.parse_args([option.option_string, '0'])
                self.parse_args([option.option_string, 'false'])
                self.parse_args([option.option_string, 'False'])
                self.parse_args([option.option_string, 'FALSE'])

                # TODO: Move to unit-tests for the action class
                # Test default value
                namespace = self.parse_args([option.option_string])
                self.assertFalse(getattr(namespace, option.dest))

                # Test setting value to True resulting in False
                namespace = self.parse_args([option.option_string, 'True'])
                self.assertFalse(getattr(namespace, option.dest))

                # Test setting value to False resulting in True
                namespace = self.parse_args([option.option_string, 'False'])
                self.assertTrue(getattr(namespace, option.dest))

        return test

    @classmethod
    def _generate_choices_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                for choice in option.choices:
                    namespace = self.parse_args(
                        [option.option_string, str(choice)])
                    self.assertEqual(getattr(namespace, option.dest), choice)

            with self.assertRaises(ParserError):
                self.parse_args([option.option_string, 'INVALID'])

        return test

    @classmethod
    def _generate_int_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                for i in [0, 1, 42]:
                    namespace = self.parse_args([option.option_string, str(i)])
                    self.assertEqual(int(getattr(namespace, option.dest)), i)

            # FIXME: int-type options should not accept non-int strings
            # with self.assertRaises(ParserError):
            #     self.parse_args([option.option_string, str(0.0)])
            #     self.parse_args([option.option_string, str(1.0)])
            #     self.parse_args([option.option_string, str(3.14)])
            #     self.parse_args([option.option_string, 'NaN'])

        return test

    @classmethod
    def _generate_str_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                self.parse_args([option.option_string, 'foo'])

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
    def _generate_append_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                # Range size is arbitrary, just needs to be more than once
                for i in range(1, 4):
                    namespace = self.parse_args(
                        [option.option_string, 'ARG'] * i)
                    self.assertEqual(getattr(namespace, option.dest),
                                     ['ARG'] * i)

        return test

    @classmethod
    def _generate_unsupported_option_test(cls, option):
        def test(self):
            with self.assertRaises(ParserError):
                self.parse_args([option.option_string])

        return test

    @classmethod
    def _generate_build_script_impl_option_test(cls, option):
        def test(self):
            with self.assertNotRaises(ParserError):
                namespace, unknown_args = self.parse_args_and_unknown_args([])
                self.assertFalse(hasattr(namespace, option.dest))
                self.assertEqual(unknown_args, [])

                namespace, unknown_args = self.parse_args_and_unknown_args(
                    [option.option_string])
                # The argument should never show up in the namespace
                self.assertFalse(hasattr(namespace, option.dest))
                # It should instead be forwareded to unkown_args
                self.assertEqual(unknown_args, [option.option_string])

        return test

    @classmethod
    def generate_option_test(cls, option):
        generate_test_funcs = {
            eo.HelpOption: cls._generate_help_option_test,
            eo.SetOption: cls._generate_set_option_test,
            eo.SetTrueOption: cls._generate_set_true_option_test,
            eo.SetFalseOption: cls._generate_set_false_option_test,
            eo.EnableOption: cls._generate_enable_option_test,
            eo.DisableOption: cls._generate_disable_option_test,
            eo.ChoicesOption: cls._generate_choices_option_test,
            eo.IntOption: cls._generate_int_option_test,
            eo.StrOption: cls._generate_str_option_test,
            eo.PathOption: cls._generate_path_option_test,
            eo.AppendOption: cls._generate_append_option_test,
            eo.UnsupportedOption: cls._generate_unsupported_option_test,
            eo.BuildScriptImplOption:
                cls._generate_build_script_impl_option_test,

            # IgnoreOptions should be manually tested
            eo.IgnoreOption: lambda self: None,
        }

        test_func = generate_test_funcs.get(option.__class__, None)
        if test_func is not None:
            return test_func(option)

        # Catch-all meaningless test
        return lambda self: \
            self.fail('unexpected option "{}"'.format(option.option_string))

    @classmethod
    def generate_preset_test(cls, preset_name, preset_args):
        def test(self):
            try:
                # Windows cannot run build-script-impl to check the impl args.
                is_windows = platform.system() == 'Windows'
                self.parse_default_args(preset_args,
                                        check_impl_args=not is_windows)
            except ParserError as e:
                self.fail('failed to parse preset "{}": {}'.format(
                    preset_name, e))

        return test


class TestDriverArgumentParser(unittest.TestCase):

    __metaclass__ = TestDriverArgumentParserMeta

    @contextmanager
    def _quiet_output(self):
        with open(os.devnull, 'w') as devnull:
            with redirect_stderr(devnull), redirect_stdout(devnull):
                yield

    def _parse_args(self, args):
        try:
            return migration.parse_args(self.parser, args)
        except (SystemExit, ValueError) as e:
            raise ParserError('failed to parse arguments: ' +
                              str(args), e)

    def _check_impl_args(self, namespace):
        assert hasattr(namespace, 'build_script_impl_args')

        try:
            migration.check_impl_args(BUILD_SCRIPT_IMPL,
                                      namespace.build_script_impl_args)
        except (SystemExit, ValueError) as e:
            raise ParserError('failed to parse impl arguments: ' +
                              str(namespace.build_script_impl_args), e)

    def parse_args_and_unknown_args(self, args, namespace=None):
        if namespace is None:
            namespace = argparse.Namespace()

        with self._quiet_output():
            try:
                namespace, unknown_args =\
                    super(self.parser.__class__, self.parser)\
                    .parse_known_args(args, namespace)
                namespace, unknown_args =\
                    migration.process_disambiguation_arguments(namespace,
                                                               unknown_args)
            except (SystemExit, argparse.ArgumentError) as e:
                raise ParserError('failed to parse arguments: ' + str(args), e)

        return namespace, unknown_args

    def parse_args(self, args, namespace=None):
        namespace, unknown_args = self.parse_args_and_unknown_args(args,
                                                                   namespace)

        if unknown_args:
            raise ParserError('unknown arguments: ' + str(unknown_args))

        return namespace

    def parse_default_args(self, args, check_impl_args=False):
        with self._quiet_output():
            namespace = self._parse_args(args)

            if check_impl_args:
                self._check_impl_args(namespace)

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

    def test_expected_options_exhaustive(self):
        """Test that we are exhaustively testing all options accepted by the
        parser. If this test if failing then the parser accepts more options
        than currently being tested, meaning the EXPECTED_OPTIONS list in
        build_swift/tests/expected_options.py should be updated to include
        the missing options.
        """

        expected_options = {o.option_string for o in eo.EXPECTED_OPTIONS}

        # aggregate and flatten the options_strings accepted by the parser
        actual_options = [a.option_strings for a in self.parser._actions]
        actual_options = set(sum(actual_options, []))

        diff = actual_options - expected_options

        if len(diff) > 0:
            self.fail('non-exhaustive expected options, missing: {}'
                      .format(diff))

    def test_expected_options_have_default_values(self):
        """Test that all the options in EXPECTED_OPTIONS have an associated
        default value.
        """

        skip_option_classes = [
            eo.HelpOption,
            eo.IgnoreOption,
            eo.UnsupportedOption,
            eo.BuildScriptImplOption,
        ]

        missing_defaults = set()
        for option in eo.EXPECTED_OPTIONS:
            if option.__class__ in skip_option_classes:
                continue

            if option.dest not in eo.EXPECTED_DEFAULTS:
                missing_defaults.add(option.dest)

        if len(missing_defaults) > 0:
            self.fail('non-exhaustive default values for options, missing: {}'
                      .format(missing_defaults))

    # -------------------------------------------------------------------------
    # Manual option tests

    def test_option_clang_compiler_version(self):
        option_string = '--clang-compiler-version'

        with self.assertNotRaises(ParserError):
            self.parse_default_args([option_string, '5.0.0'])
            self.parse_default_args([option_string, '5.0.1'])
            self.parse_default_args([option_string, '5.0.0.1'])

        with self.assertRaises(ParserError):
            self.parse_default_args([option_string, '1'])
            self.parse_default_args([option_string, '1.2'])
            self.parse_default_args([option_string, '0.0.0.0.1'])

    def test_option_clang_user_visible_version(self):
        option_string = '--clang-user-visible-version'

        with self.assertNotRaises(ParserError):
            self.parse_default_args([option_string, '5.0.0'])
            self.parse_default_args([option_string, '5.0.1'])
            self.parse_default_args([option_string, '5.0.0.1'])

        with self.assertRaises(ParserError):
            self.parse_default_args([option_string, '1'])
            self.parse_default_args([option_string, '1.2'])
            self.parse_default_args([option_string, '0.0.0.0.1'])

    def test_option_swift_compiler_version(self):
        option_string = '--swift-compiler-version'

        with self.assertNotRaises(ParserError):
            self.parse_default_args([option_string, '4.1'])
            self.parse_default_args([option_string, '4.0.1'])
            self.parse_default_args([option_string, '200.99.1'])

        with self.assertRaises(ParserError):
            self.parse_default_args([option_string, '1'])
            self.parse_default_args([option_string, '0.0.0.1'])

    def test_option_swift_user_visible_version(self):
        option_string = '--swift-user-visible-version'

        with self.assertNotRaises(ParserError):
            self.parse_default_args([option_string, '4.1'])
            self.parse_default_args([option_string, '4.0.1'])
            self.parse_default_args([option_string, '200.99.1'])

        with self.assertRaises(ParserError):
            self.parse_default_args([option_string, '1'])
            self.parse_default_args([option_string, '0.0.0.1'])

    def test_option_I(self):
        with self.assertRaises(ValueError):
            self.parse_default_args(['-I'])

    def test_option_ios_all(self):
        with self.assertRaises(ValueError):
            self.parse_default_args(['--ios-all'])

    def test_option_tvos_all(self):
        with self.assertRaises(ValueError):
            self.parse_default_args(['--tvos-all'])

    def test_option_watchos_all(self):
        with self.assertRaises(ValueError):
            self.parse_default_args(['--watchos-all'])

    # -------------------------------------------------------------------------
    # Implied defaults tests

    def test_implied_defaults_assertions(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--assertions'])

            self.assertTrue(namespace.cmark_assertions)
            self.assertTrue(namespace.llvm_assertions)
            self.assertTrue(namespace.swift_assertions)
            self.assertTrue(namespace.swift_stdlib_assertions)

    def test_implied_defaults_cmark_build_variant(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--debug-cmark'])
            self.assertTrue(namespace.build_cmark)

    def test_implied_defaults_lldb_build_variant(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--debug-lldb'])
            self.assertTrue(namespace.build_lldb)

        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--lldb-assertions'])
            self.assertTrue(namespace.build_lldb)

    def test_implied_defaults_build_variant(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--debug'])

            self.assertEqual(namespace.cmark_build_variant, 'Debug')
            self.assertEqual(namespace.foundation_build_variant, 'Debug')
            self.assertEqual(namespace.libdispatch_build_variant, 'Debug')
            self.assertEqual(namespace.libicu_build_variant, 'Debug')
            self.assertEqual(namespace.lldb_build_variant, 'Debug')
            self.assertEqual(namespace.llvm_build_variant, 'Debug')
            self.assertEqual(namespace.swift_build_variant, 'Debug')
            self.assertEqual(namespace.swift_stdlib_build_variant, 'Debug')

    def test_implied_defaults_skip_build(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--skip-build'])

            self.assertFalse(namespace.build_benchmarks)

            self.assertFalse(namespace.build_linux)
            self.assertFalse(namespace.build_android)
            self.assertFalse(namespace.build_freebsd)
            self.assertFalse(namespace.build_cygwin)
            self.assertFalse(namespace.build_osx)
            self.assertFalse(namespace.build_ios)
            self.assertFalse(namespace.build_tvos)
            self.assertFalse(namespace.build_watchos)

            self.assertFalse(namespace.build_foundation)
            self.assertFalse(namespace.build_libdispatch)
            self.assertFalse(namespace.build_libicu)
            self.assertFalse(namespace.build_lldb)
            self.assertFalse(namespace.build_llbuild)
            self.assertFalse(namespace.build_libcxx)
            self.assertFalse(namespace.build_playgroundsupport)
            self.assertFalse(namespace.build_swiftpm)
            self.assertFalse(namespace.build_xctest)

    def test_implied_defaults_skip_build_ios(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--skip-build-ios'])
            self.assertFalse(namespace.build_ios_device)
            self.assertFalse(namespace.build_ios_simulator)

            # Also implies that the tests should be skipped
            self.assertFalse(namespace.test_ios_host)
            self.assertFalse(namespace.test_ios_simulator)

    def test_implied_defaults_skip_build_tvos(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--skip-build-tvos'])
            self.assertFalse(namespace.build_tvos_device)
            self.assertFalse(namespace.build_tvos_simulator)

            # Also implies that the tests should be skipped
            self.assertFalse(namespace.test_tvos_host)
            self.assertFalse(namespace.test_tvos_simulator)

    def test_implied_defaults_skip_build_watchos(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--skip-build-watchos'])
            self.assertFalse(namespace.build_watchos_device)
            self.assertFalse(namespace.build_watchos_simulator)

            # Also implies that the tests should be skipped
            self.assertFalse(namespace.test_watchos_host)
            self.assertFalse(namespace.test_watchos_simulator)

    def test_implied_defaults_validation_test(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--validation-test'])
            self.assertTrue(namespace.test)

    def test_implied_defaults_test_optimized(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--test-optimized'])
            self.assertTrue(namespace.test)

    def test_implied_defaults_test_optimize_for_size(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--test-optimize-for-size'])
            self.assertTrue(namespace.test)

    def test_implied_defaults_test_optimize_none_with_implicit_dynamic(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(
                ['--test-optimize-none-with-implicit-dynamic'])
            self.assertTrue(namespace.test)

    def test_implied_defaults_skip_all_tests(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args([
                '--test', '0',
                '--validation-test', '0',
                '--long-test', '0',
                '--stress-test', '0',
            ])

            self.assertFalse(namespace.test_linux)
            self.assertFalse(namespace.test_freebsd)
            self.assertFalse(namespace.test_cygwin)
            self.assertFalse(namespace.test_osx)
            self.assertFalse(namespace.test_ios)
            self.assertFalse(namespace.test_tvos)
            self.assertFalse(namespace.test_watchos)

    def test_implied_defaults_skip_test_ios(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--skip-test-ios'])
            self.assertFalse(namespace.test_ios_host)
            self.assertFalse(namespace.test_ios_simulator)

    def test_implied_defaults_skip_test_tvos(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--skip-test-tvos'])
            self.assertFalse(namespace.test_tvos_host)
            self.assertFalse(namespace.test_tvos_simulator)

    def test_implied_defaults_skip_test_watchos(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--skip-test-watchos'])
            self.assertFalse(namespace.test_watchos_host)
            self.assertFalse(namespace.test_watchos_simulator)

    def test_implied_defaults_skip_build_android(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--android', '0'])
            self.assertFalse(namespace.test_android_host)

        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--skip-build-android'])
            self.assertFalse(namespace.test_android_host)

    def test_implied_defaults_host_test(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--host-test', '0'])
            self.assertFalse(namespace.test_ios_host)
            self.assertFalse(namespace.test_tvos_host)
            self.assertFalse(namespace.test_watchos_host)
            self.assertFalse(namespace.test_android_host)
            self.assertFalse(namespace.build_libparser_only)

    def test_build_lib_swiftsyntaxparser_only(self):
        with self.assertNotRaises(ParserError):
            namespace = self.parse_default_args(['--build-libparser-only'])
            self.assertTrue(namespace.build_libparser_only)


if __name__ == '__main__':
    unittest.main()
