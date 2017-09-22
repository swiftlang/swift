# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import argparse


__all__ = [
    'Option',
    'AppendOption',
    'ChoicesOption',
    'HelpOption',
    'IgnoreOption',
    'IntOption',
    'PathOption',
    'StrOption',
    'ToggleOption',
    'UnsupportedOption',
    'EXPECTED_OPTIONS',
    'EXPECTED_DEFAULTS',
]


# -----------------------------------------------------------------------------

EXPECTED_DEFAULTS = {
    'android': False,
    'android_api_level': '21',
    'android_deploy_device_path': '/data/local/tmp',
    'android_icu_i18n': None,
    'android_icu_i18n_include': None,
    'android_icu_uc': None,
    'android_icu_uc_include': None,
    'android_ndk': None,
    'android_ndk_gcc_version': '4.9',
    'assertions': True,
    'benchmark': False,
    'benchmark_num_o_iterations': 3,
    'benchmark_num_onone_iterations': 3,
    'build_android': False,
    'build_args': [],
    'build_benchmarks': True,
    'build_cygwin': True,
    'build_foundation': False,
    'build_freebsd': True,
    'build_ios': True,
    'build_ios_device': False,
    'build_ios_simulator': False,
    'build_jobs': 4,
    'build_libdispatch': False,
    'build_libicu': False,
    'build_linux': True,
    'build_llbuild': False,
    'build_lldb': False,
    'build_ninja': False,
    'build_osx': True,
    'build_playgroundlogger': False,
    'build_playgroundsupport': False,
    'build_runtime_with_host_compiler': False,
    'build_stdlib_deployment_targets': ['all'],
    'build_subdir': 'Ninja-DebugAssert',
    'build_swift_dynamic_sdk_overlay': True,
    'build_swift_dynamic_stdlib': True,
    'build_swift_static_sdk_overlay': False,
    'build_swift_static_stdlib': False,
    'build_swift_stdlib_unittest_extra': False,
    'build_swiftpm': False,
    'build_tvos': True,
    'build_tvos_device': False,
    'build_tvos_simulator': False,
    'build_variant': 'Debug',
    'build_watchos': True,
    'build_watchos_device': False,
    'build_watchos_simulator': False,
    'build_xctest': False,
    'clang_compiler_version': None,
    'clang_profile_instr_use': None,
    'clang_user_visible_version': '5.0.0',
    'clean': False,
    'cmake': None,
    'cmake_generator': 'Ninja',
    'cmark_assertions': True,
    'cmark_build_variant': 'Debug',
    'compiler_vendor': 'none',
    'coverage_db': None,
    'cross_compile_hosts': [],
    'darwin_deployment_version_ios': '7.0',
    'darwin_deployment_version_osx': '10.9',
    'darwin_deployment_version_tvos': '9.0',
    'darwin_deployment_version_watchos': '2.0',
    'darwin_xcrun_toolchain': 'default',
    'distcc': False,
    'dry_run': False,
    'enable_asan': False,
    'enable_lsan': False,
    'enable_sil_ownership': False,
    'enable_tsan': False,
    'enable_tsan_runtime': None,
    'enable_ubsan': False,
    'export_compile_commands': False,
    'extra_cmake_options': [],
    'extra_swift_args': [],
    'force_optimized_typechecker': False,
    'foundation_build_variant': 'Debug',
    'host_cc': None,
    'host_cxx': None,
    'host_libtool': None,
    'host_lipo': None,
    'host_target': 'macosx-x86_64',
    'host_test': False,
    'install_prefix': '/Applications/Xcode.app/Contents/Developer/Toolchains/'
                      'XcodeDefault.xctoolchain/usr',
    'install_symroot': None,
    'ios': False,
    'ios_all': False,
    'legacy_impl': True,
    'libdispatch_build_variant': 'Debug',
    'libicu_build_variant': 'Debug',
    'lit_args': '-sv',
    'lldb_assertions': None,
    'lldb_build_variant': 'Debug',
    'llvm_assertions': True,
    'llvm_build_variant': 'Debug',
    'llvm_max_parallel_lto_link_jobs': 0,
    'llvm_targets_to_build': 'X86;ARM;AArch64;PowerPC;SystemZ;Mips',
    'long_test': False,
    'lto_type': None,
    'show_sdks': False,
    'skip_build': False,
    'stdlib_deployment_targets': [
        'macosx-x86_64',
        'iphonesimulator-i386',
        'iphonesimulator-x86_64',
        'appletvsimulator-x86_64',
        'watchsimulator-i386',
        'iphoneos-armv7',
        'iphoneos-armv7s',
        'iphoneos-arm64',
        'appletvos-arm64',
        'watchos-armv7k'
    ],
    'swift_analyze_code_coverage': 'false',
    'swift_assertions': True,
    'swift_build_variant': 'Debug',
    'swift_compiler_version': None,
    'swift_stdlib_assertions': True,
    'swift_stdlib_build_variant': 'Debug',
    'swift_tools_max_parallel_lto_link_jobs': 0,
    'swift_user_visible_version': '4.1',
    'symbols_package': None,
    'test': None,
    'test_android_device': False,
    'test_cygwin': False,
    'test_freebsd': False,
    'test_ios': False,
    'test_ios_32bit_simulator': True,
    'test_ios_device': False,
    'test_ios_simulator': False,
    'test_linux': False,
    'test_optimize_for_size': None,
    'test_optimized': None,
    'test_osx': False,
    'test_paths': [],
    'test_tvos': False,
    'test_tvos_device': False,
    'test_tvos_simulator': False,
    'test_watchos': False,
    'test_watchos_device': False,
    'test_watchos_simulator': False,
    'tvos': False,
    'tvos_all': False,
    'validation_test': None,
    'verbose_build': False,
    'watchos': False,
    'watchos_all': False
}


# -----------------------------------------------------------------------------

class _BaseOption(object):

    def __init__(self, option_string, dest, default=None):
        self.option_string = option_string
        self.dest = dest

        if default is None:
            default = EXPECTED_DEFAULTS.get(dest, None)

        self.default = default

    def sanitized_str(self):
        if self.option_string.startswith('--'):
            return self.option_string[2:].replace('-', '_')

        if len(self.option_string) == 2 and self.option_string[0] == '-':
            return self.option_string[1]

        raise ValueError('invalid option_string format: ' + self.option_string)


class Option(_BaseOption):
    """Option that accepts no arguments."""

    def __init__(self, *args, **kwargs):
        self.value = kwargs.pop('value', None)
        super(Option, self).__init__(*args, **kwargs)


class HelpOption(_BaseOption):
    """Option that prints the help message and exits."""

    pass


class ToggleOption(_BaseOption):
    """Option that accepts no argument or an optional bool argument."""

    pass


class ChoicesOption(_BaseOption):
    """Option that accepts an argument from a predifined list of choices."""

    def __init__(self, *args, **kwargs):
        self.choices = kwargs.pop('choices', None)
        super(ChoicesOption, self).__init__(*args, **kwargs)


class IntOption(_BaseOption):
    """Option that accepts an int argument."""

    pass


class StrOption(_BaseOption):
    """Option that accepts a str argument."""

    pass


class PathOption(_BaseOption):
    """Option that accepts a path argument."""

    pass


class AppendOption(_BaseOption):
    """Option that can be called more than once to append argument to internal
    list.
    """

    pass


class UnsupportedOption(_BaseOption):
    """Option that is not supported."""

    def __init__(self, *args, **kwargs):
        kwargs['dest'] = kwargs.pop('dest', None)
        super(UnsupportedOption, self).__init__(*args, **kwargs)


class IgnoreOption(_BaseOption):
    """Option that should be ignored when generating tests. Instead a test
    should be written manually as the behavior cannot or should not be auto-
    generated.
    """

    pass


# -----------------------------------------------------------------------------

EXPECTED_OPTIONS = [
    # Ignore the help options since they always call sys.exit(0)
    HelpOption('-h', dest='help', default=argparse.SUPPRESS),
    HelpOption('--help', dest='help', default=argparse.SUPPRESS),

    Option('--assertions', dest='assertions', value=True),
    Option('--benchmark', dest='benchmark', value=True),
    Option('--clean', dest='clean', value=True),
    Option('--cmark-assertions', dest='cmark_assertions', value=True),
    Option('--debug', dest='build_variant', value='Debug'),
    Option('--debug-cmark', dest='cmark_build_variant', value='Debug'),
    Option('--debug-foundation',
           dest='foundation_build_variant', value='Debug'),
    Option('--debug-libdispatch',
           dest='libdispatch_build_variant', value='Debug'),
    Option('--debug-libicu', dest='libicu_build_variant', value='Debug'),
    Option('--debug-lldb', dest='lldb_build_variant', value='Debug'),
    Option('--debug-llvm', dest='llvm_build_variant', value='Debug'),
    Option('--debug-swift', dest='swift_build_variant', value='Debug'),
    Option('--debug-swift-stdlib',
           dest='swift_stdlib_build_variant', value='Debug'),
    Option('--dry-run', dest='dry_run', value=True),
    Option('--eclipse', dest='cmake_generator', value='Eclipse CDT4 - Ninja'),
    Option('--enable-sil-ownership', dest='enable_sil_ownership', value=True),
    Option('--force-optimized-typechecker',
           dest='force_optimized_typechecker', value=True),
    Option('--ios', dest='ios', value=True),
    Option('--llbuild', dest='build_llbuild', value=True),
    Option('--lldb', dest='build_lldb', value=True),
    Option('--lldb-assertions', dest='lldb_assertions', value=True),
    Option('--llvm-assertions', dest='llvm_assertions', value=True),
    Option('--make', dest='cmake_generator', value='Unix Makefiles'),
    Option('--no-assertions', dest='assertions', value=False),
    Option('--no-legacy-impl', dest='legacy_impl', value=False),
    Option('--no-lldb-assertions', dest='lldb_assertions', value=False),
    Option('--no-llvm-assertions', dest='llvm_assertions', value=False),
    Option('--no-swift-assertions', dest='swift_assertions', value=False),
    Option('--no-swift-stdlib-assertions',
           dest='swift_stdlib_assertions', value=False),
    Option('--playgroundlogger', dest='build_playgroundlogger', value=True),
    Option('--playgroundsupport', dest='build_playgroundsupport', value=True),
    Option('--release', dest='build_variant', value='Release'),
    Option('--release-debuginfo',
           dest='build_variant', value='RelWithDebInfo'),
    Option('--skip-build', dest='skip_build', value=True),
    Option('--skip-ios', dest='ios', value=False),
    Option('--skip-tvos', dest='tvos', value=False),
    Option('--skip-watchos', dest='watchos', value=False),
    Option('--swift-assertions', dest='swift_assertions', value=True),
    Option('--swift-stdlib-assertions',
           dest='swift_stdlib_assertions', value=True),
    Option('--swiftpm', dest='build_swiftpm', value=True),
    Option('--xcode', dest='cmake_generator', value='Xcode'),
    Option('-B', dest='benchmark', value=True),
    Option('-R', dest='build_variant', value='Release'),
    Option('-S', dest='skip_build', value=True),
    Option('-T', dest='validation_test', value=True),
    Option('-b', dest='build_llbuild', value=True),
    Option('-c', dest='clean', value=True),
    Option('-d', dest='build_variant', value='Debug'),
    Option('-e', dest='cmake_generator', value='Eclipse CDT4 - Ninja'),
    Option('-i', dest='ios', value=True),
    Option('-l', dest='build_lldb', value=True),
    Option('-m', dest='cmake_generator', value='Unix Makefiles'),
    Option('-n', dest='dry_run', value=True),
    Option('-o', dest='test_optimized', value=True),
    Option('-p', dest='build_swiftpm', value=True),
    Option('-r', dest='build_variant', value='RelWithDebInfo'),
    Option('-s', dest='test_optimize_for_size', value=True),
    Option('-t', dest='test', value=True),
    Option('-x', dest='cmake_generator', value='Xcode'),

    ToggleOption('--android', dest='android'),
    ToggleOption('--build-ninja', dest='build_ninja'),
    ToggleOption('--build-runtime-with-host-compiler',
                 dest='build_runtime_with_host_compiler'),
    ToggleOption('--build-swift-dynamic-sdk-overlay',
                 dest='build_swift_dynamic_sdk_overlay'),
    ToggleOption('--build-swift-dynamic-stdlib',
                 dest='build_swift_dynamic_stdlib'),
    ToggleOption('--build-swift-static-sdk-overlay',
                 dest='build_swift_static_sdk_overlay'),
    ToggleOption('--build-swift-static-stdlib',
                 dest='build_swift_static_stdlib'),
    ToggleOption('--build-swift-stdlib-unittest-extra',
                 dest='build_swift_stdlib_unittest_extra'),
    ToggleOption('--distcc', dest='distcc'),
    ToggleOption('--enable-asan', dest='enable_asan'),
    ToggleOption('--enable-lsan', dest='enable_lsan'),
    ToggleOption('--enable-tsan', dest='enable_tsan'),
    ToggleOption('--enable-ubsan', dest='enable_ubsan'),
    ToggleOption('--export-compile-commands', dest='export_compile_commands'),
    ToggleOption('--foundation', dest='build_foundation'),
    ToggleOption('--host-test', dest='host_test'),
    ToggleOption('--libdispatch', dest='build_libdispatch'),
    ToggleOption('--libicu', dest='build_libicu'),
    ToggleOption('--long-test', dest='long_test'),
    ToggleOption('--show-sdks', dest='show_sdks'),
    ToggleOption('--skip-build-android', dest='build_android'),
    ToggleOption('--skip-build-benchmarks', dest='build_benchmarks'),
    ToggleOption('--skip-build-cygwin', dest='build_cygwin'),
    ToggleOption('--skip-build-freebsd', dest='build_freebsd'),
    ToggleOption('--skip-build-ios', dest='build_ios'),
    ToggleOption('--skip-build-ios-device', dest='build_ios_device'),
    ToggleOption('--skip-build-ios-simulator',
                 dest='build_ios_simulator'),
    ToggleOption('--skip-build-linux', dest='build_linux'),
    ToggleOption('--skip-build-osx', dest='build_osx'),
    ToggleOption('--skip-build-tvos', dest='build_tvos'),
    ToggleOption('--skip-build-tvos-device', dest='build_tvos_device'),
    ToggleOption('--skip-build-tvos-simulator',
                 dest='build_tvos_simulator'),
    ToggleOption('--skip-build-watchos', dest='build_watchos'),
    ToggleOption('--skip-build-watchos-device',
                 dest='build_watchos_device'),
    ToggleOption('--skip-build-watchos-simulator',
                 dest='build_watchos_simulator'),
    ToggleOption('--skip-test-android-host', dest='test_android_device'),
    ToggleOption('--skip-test-cygwin', dest='test_cygwin'),
    ToggleOption('--skip-test-freebsd', dest='test_freebsd'),
    ToggleOption('--skip-test-ios', dest='test_ios'),
    ToggleOption('--skip-test-ios-32bit-simulator',
                 dest='test_ios_32bit_simulator'),
    ToggleOption('--skip-test-ios-host', dest='test_ios_device'),
    ToggleOption('--skip-test-ios-simulator', dest='test_ios_simulator'),
    ToggleOption('--skip-test-linux', dest='test_linux'),
    ToggleOption('--skip-test-osx', dest='test_osx'),
    ToggleOption('--skip-test-tvos', dest='test_tvos'),
    ToggleOption('--skip-test-tvos-host', dest='test_tvos_device'),
    ToggleOption('--skip-test-tvos-simulator',
                 dest='test_tvos_simulator'),
    ToggleOption('--skip-test-watchos', dest='test_watchos'),
    ToggleOption('--skip-test-watchos-host', dest='test_watchos_device'),
    ToggleOption('--skip-test-watchos-simulator',
                 dest='test_watchos_simulator'),
    ToggleOption('--test', dest='test'),
    ToggleOption('--test-optimize-for-size', dest='test_optimize_for_size'),
    ToggleOption('--test-optimized', dest='test_optimized'),
    ToggleOption('--tvos', dest='tvos'),
    ToggleOption('--validation-test', dest='validation_test'),
    ToggleOption('--verbose-build', dest='verbose_build'),
    ToggleOption('--watchos', dest='watchos'),
    ToggleOption('--xctest', dest='build_xctest'),

    ChoicesOption('--android-ndk-gcc-version',
                  dest='android_ndk_gcc_version',
                  choices=['4.8', '4.9']),
    ChoicesOption('--compiler-vendor',
                  dest='compiler_vendor',
                  choices=['none', 'apple']),
    ChoicesOption('--swift-analyze-code-coverage',
                  dest='swift_analyze_code_coverage',
                  choices=['false', 'not-merged', 'merged']),

    StrOption('--android-api-level', dest='android_api_level'),
    StrOption('--build-args', dest='build_args'),
    StrOption('--build-stdlib-deployment-targets',
              dest='build_stdlib_deployment_targets'),
    StrOption('--darwin-deployment-version-ios',
              dest='darwin_deployment_version_ios'),
    StrOption('--darwin-deployment-version-osx',
              dest='darwin_deployment_version_osx'),
    StrOption('--darwin-deployment-version-tvos',
              dest='darwin_deployment_version_tvos'),
    StrOption('--darwin-deployment-version-watchos',
              dest='darwin_deployment_version_watchos'),
    StrOption('--darwin-xcrun-toolchain', dest='darwin_xcrun_toolchain'),
    StrOption('--enable-tsan-runtime', dest='enable_tsan_runtime'),
    StrOption('--host-target', dest='host_target'),
    StrOption('--lit-args', dest='lit_args'),
    StrOption('--llvm-targets-to-build', dest='llvm_targets_to_build'),

    PathOption('--android-deploy-device-path',
               dest='android_deploy_device_path'),
    PathOption('--android-icu-i18n', dest='android_icu_i18n'),
    PathOption('--android-icu-i18n-include', dest='android_icu_i18n_include'),
    PathOption('--android-icu-uc', dest='android_icu_uc'),
    PathOption('--android-icu-uc-include', dest='android_icu_uc_include'),
    PathOption('--android-ndk', dest='android_ndk'),
    PathOption('--build-subdir', dest='build_subdir'),
    PathOption('--clang-profile-instr-use', dest='clang_profile_instr_use'),
    PathOption('--cmake', dest='cmake'),
    PathOption('--coverage-db', dest='coverage_db'),
    PathOption('--host-cc', dest='host_cc'),
    PathOption('--host-cxx', dest='host_cxx'),
    PathOption('--host-libtool', dest='host_libtool'),
    PathOption('--host-lipo', dest='host_lipo'),
    PathOption('--install-prefix', dest='install_prefix'),
    PathOption('--install-symroot', dest='install_symroot'),
    PathOption('--symbols-package', dest='symbols_package'),

    IntOption('--benchmark-num-o-iterations',
              dest='benchmark_num_o_iterations'),
    IntOption('--benchmark-num-onone-iterations',
              dest='benchmark_num_onone_iterations'),
    IntOption('--jobs', dest='build_jobs'),
    IntOption('--llvm-max-parallel-lto-link-jobs',
              dest='llvm_max_parallel_lto_link_jobs'),
    IntOption('--swift-tools-max-parallel-lto-link-jobs',
              dest='swift_tools_max_parallel_lto_link_jobs'),
    IntOption('-j', dest='build_jobs'),

    AppendOption('--cross-compile-hosts', dest='cross_compile_hosts'),
    AppendOption('--extra-cmake-options', dest='extra_cmake_options'),
    AppendOption('--extra-swift-args', dest='extra_swift_args'),
    AppendOption('--stdlib-deployment-targets',
                 dest='stdlib_deployment_targets'),
    AppendOption('--test-paths', dest='test_paths'),

    UnsupportedOption('--build-jobs'),
    UnsupportedOption('--common-cmake-options'),
    UnsupportedOption('--ios-all'),
    UnsupportedOption('--only-execute'),
    UnsupportedOption('--skip-test-optimize-for-size'),
    UnsupportedOption('--skip-test-optimized'),
    UnsupportedOption('--tvos-all'),
    UnsupportedOption('--watchos-all'),
    UnsupportedOption('-I'),

    # NOTE: LTO flag is a special case that acts both as an option and has
    # valid choices
    Option('--lto', dest='lto_type'),
    ChoicesOption('--lto', dest='lto_type', choices=['thin', 'full']),

    # NOTE: We'll need to manually test the behavior of these since they
    # validate compiler version strings.
    IgnoreOption('--clang-compiler-version',
                 dest='clang_compiler_version'),
    IgnoreOption('--clang-user-visible-version',
                 dest='clang_user_visible_version'),
    IgnoreOption('--swift-compiler-version',
                 dest='swift_compiler_version'),
    IgnoreOption('--swift-user-visible-version',
                 dest='swift_user_visible_version'),
]
