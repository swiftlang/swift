# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


import multiprocessing

from swift_build_support.swift_build_support import host
from swift_build_support.swift_build_support import targets

from .. import argparse
from .. import defaults


__all__ = [
    'HelpOption',
    'SetOption',
    'SetTrueOption',
    'SetFalseOption',
    'DisableOption',
    'EnableOption',
    'ChoicesOption',
    'IntOption',
    'StrOption',
    'PathOption',
    'AppendOption',
    'UnsupportedOption',
    'IgnoreOption',
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
    'build_external_benchmarks': False,
    'build_foundation': False,
    'build_freebsd': True,
    'build_ios': True,
    'build_ios_device': False,
    'build_ios_simulator': False,
    'build_jobs': multiprocessing.cpu_count(),
    'build_libdispatch': False,
    'build_libicu': False,
    'build_linux': True,
    'build_llbuild': False,
    'build_lldb': False,
    'build_ninja': False,
    'build_osx': True,
    'build_playgroundsupport': False,
    'build_runtime_with_host_compiler': False,
    'build_stdlib_deployment_targets': ['all'],
    'build_subdir': None,
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
    'clang_user_visible_version': defaults.CLANG_USER_VISIBLE_VERSION,
    'clean': False,
    'cmake': None,
    'cmake_generator': 'Ninja',
    'cmark_assertions': True,
    'cmark_build_variant': 'Debug',
    'compiler_vendor': defaults.COMPILER_VENDOR,
    'coverage_db': None,
    'cross_compile_hosts': [],
    'darwin_deployment_version_ios':
        defaults.DARWIN_DEPLOYMENT_VERSION_IOS,
    'darwin_deployment_version_osx':
        defaults.DARWIN_DEPLOYMENT_VERSION_OSX,
    'darwin_deployment_version_tvos':
        defaults.DARWIN_DEPLOYMENT_VERSION_TVOS,
    'darwin_deployment_version_watchos':
        defaults.DARWIN_DEPLOYMENT_VERSION_WATCHOS,
    'darwin_xcrun_toolchain': None,
    'distcc': False,
    'dry_run': False,
    'enable_asan': False,
    'enable_lsan': False,
    'enable_sanitize_coverage': False,
    'enable_sil_ownership': False,
    'disable_guaranteed_normal_arguments': False,
    'enable_stdlibcore_exclusivity_checking': False,
    'enable_tsan': False,
    'enable_tsan_runtime': False,
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
    'host_target': targets.StdlibDeploymentTarget.host_target().name,
    'host_test': False,
    'install_prefix': targets.install_prefix(),
    'install_symroot': None,
    'ios': False,
    'ios_all': False,
    'legacy_impl': True,
    'libdispatch_build_variant': 'Debug',
    'libicu_build_variant': 'Debug',
    'lit_args': '-sv',
    'llbuild_assertions': True,
    'lldb_assertions': True,
    'lldb_build_variant': 'Debug',
    'lldb_build_with_xcode': '1',
    'llvm_assertions': True,
    'llvm_build_variant': 'Debug',
    'llvm_max_parallel_lto_link_jobs':
        host.max_lto_link_job_counts()['llvm'],
    'llvm_targets_to_build': 'X86;ARM;AArch64;PowerPC;SystemZ;Mips',
    'long_test': False,
    'lto_type': None,
    'show_sdks': False,
    'skip_build': False,
    'stdlib_deployment_targets': None,
    'stress_test': False,
    'swift_analyze_code_coverage': defaults.SWIFT_ANALYZE_CODE_COVERAGE,
    'swift_assertions': True,
    'swift_build_variant': 'Debug',
    'swift_compiler_version': None,
    'swift_stdlib_assertions': True,
    'swift_stdlib_build_variant': 'Debug',
    'swift_tools_max_parallel_lto_link_jobs':
        host.max_lto_link_job_counts()['swift'],
    'swift_user_visible_version': defaults.SWIFT_USER_VISIBLE_VERSION,
    'symbols_package': None,
    'test': None,
    'test_android_host': False,
    'test_cygwin': False,
    'test_freebsd': False,
    'test_ios': False,
    'test_ios_32bit_simulator': True,
    'test_ios_host': False,
    'test_ios_simulator': False,
    'test_linux': False,
    'test_optimize_for_size': None,
    'test_optimized': None,
    'test_osx': False,
    'test_paths': [],
    'test_tvos': False,
    'test_tvos_host': False,
    'test_tvos_simulator': False,
    'test_watchos': False,
    'test_watchos_host': False,
    'test_watchos_simulator': False,
    'tvos': False,
    'tvos_all': False,
    'validation_test': None,
    'verbose_build': False,
    'watchos': False,
    'watchos_all': False
}


# -----------------------------------------------------------------------------

def _sanitize_option_string(option_string):
    if option_string.startswith('--'):
        return option_string[2:].replace('-', '_')

    if len(option_string) == 2 and option_string[0] == '-':
        return option_string[1]

    raise ValueError('invalid option_string format: ' + option_string)


class _BaseOption(object):

    def __init__(self, option_string, dest=None, default=None):
        if dest is None:
            dest = _sanitize_option_string(option_string)

        if default is None:
            default = EXPECTED_DEFAULTS.get(dest, None)

        self.option_string = option_string
        self.dest = dest
        self.default = default

    def sanitized_string(self):
        return _sanitize_option_string(self.option_string)


class HelpOption(_BaseOption):
    """Option that prints the help message and exits."""

    pass


class SetOption(_BaseOption):
    """Option that accepts no arguments, setting the destination to a
    hard-coded value or None.
    """

    def __init__(self, *args, **kwargs):
        self.value = kwargs.pop('value', None)
        super(SetOption, self).__init__(*args, **kwargs)


class SetTrueOption(_BaseOption):
    """Option that accepts no arguments, setting the destination value to True
    if parsed and defaulting to False otherwise.
    """

    pass


class SetFalseOption(_BaseOption):
    """Option that accepts no arguments, setting the destination value to False
    if parsed and defaulting to True otherwise.
    """

    pass


class EnableOption(_BaseOption):
    """Option that sets the destination to True when parsed and False by default.
    Can be toggled True or False with an optional bool argument.
    """

    pass


class DisableOption(_BaseOption):
    """Option that sets the destination to False when parsed and True by default.
    Can be toggled True or False with an optional bool argument, which is then
    negated. Thus if an option is passed the value 'True' it will set the
    destination to False and vice versa.
    """

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

    pass


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

    SetOption('--debug', dest='build_variant', value='Debug'),
    SetOption('--debug-cmark', dest='cmark_build_variant', value='Debug'),
    SetOption('--debug-foundation',
              dest='foundation_build_variant', value='Debug'),
    SetOption('--debug-libdispatch',
              dest='libdispatch_build_variant', value='Debug'),
    SetOption('--debug-libicu', dest='libicu_build_variant', value='Debug'),
    SetOption('--debug-lldb', dest='lldb_build_variant', value='Debug'),
    SetOption('--lldb-build-with-xcode', dest='lldb_build_with_xcode',
              value='1'),
    SetOption('--lldb-build-with-cmake', dest='lldb_build_with_xcode',
              value='0'),
    SetOption('--debug-llvm', dest='llvm_build_variant', value='Debug'),
    SetOption('--debug-swift', dest='swift_build_variant', value='Debug'),
    SetOption('--debug-swift-stdlib',
              dest='swift_stdlib_build_variant', value='Debug'),
    SetOption('--eclipse',
              dest='cmake_generator', value='Eclipse CDT4 - Ninja'),
    SetOption('--make', dest='cmake_generator', value='Unix Makefiles'),
    SetOption('--release', dest='build_variant', value='Release'),
    SetOption('--release-debuginfo',
              dest='build_variant', value='RelWithDebInfo'),
    SetOption('--xcode', dest='cmake_generator', value='Xcode'),
    SetOption('-R', dest='build_variant', value='Release'),
    SetOption('-d', dest='build_variant', value='Debug'),
    SetOption('-e', dest='cmake_generator', value='Eclipse CDT4 - Ninja'),
    SetOption('-m', dest='cmake_generator', value='Unix Makefiles'),
    SetOption('-r', dest='build_variant', value='RelWithDebInfo'),
    SetOption('-x', dest='cmake_generator', value='Xcode'),

    # FIXME: Convert these options to set_true actions
    SetOption('--assertions', value=True),
    SetOption('--cmark-assertions', value=True),
    SetOption('--lldb-assertions', value=True),
    SetOption('--llvm-assertions', value=True),
    SetOption('--llbuild-assertions', value=True),
    SetOption('--swift-assertions', value=True),
    SetOption('--swift-stdlib-assertions', value=True),
    SetOption('-T', dest='validation_test', value=True),
    SetOption('-o', dest='test_optimized', value=True),
    SetOption('-s', dest='test_optimize_for_size', value=True),
    SetOption('-t', dest='test', value=True),

    # FIXME: Convert these options to set_false actions
    SetOption('--no-assertions', dest='assertions', value=False),
    SetOption('--no-lldb-assertions', dest='lldb_assertions', value=False),
    SetOption('--no-llvm-assertions', dest='llvm_assertions', value=False),
    SetOption('--no-llbuild-assertions',
              dest='llbuild_assertions', value=False),
    SetOption('--no-swift-assertions', dest='swift_assertions', value=False),
    SetOption('--no-swift-stdlib-assertions',
              dest='swift_stdlib_assertions', value=False),
    SetOption('--skip-ios', dest='ios', value=False),
    SetOption('--skip-tvos', dest='tvos', value=False),
    SetOption('--skip-watchos', dest='watchos', value=False),

    SetTrueOption('--benchmark'),
    SetTrueOption('--clean'),
    SetTrueOption('--dry-run'),
    SetTrueOption('--enable-sil-ownership'),
    SetTrueOption('--disable-guaranteed-normal-arguments'),
    SetTrueOption('--enable-stdlibcore-exclusivity-checking'),
    SetTrueOption('--force-optimized-typechecker'),
    SetTrueOption('--ios'),
    SetTrueOption('--llbuild', dest='build_llbuild'),
    SetTrueOption('--lldb', dest='build_lldb'),
    SetTrueOption('--playgroundsupport', dest='build_playgroundsupport'),
    SetTrueOption('--skip-build'),
    SetTrueOption('--swiftpm', dest='build_swiftpm'),
    SetTrueOption('-B', dest='benchmark'),
    SetTrueOption('-S', dest='skip_build'),
    SetTrueOption('-b', dest='build_llbuild'),
    SetTrueOption('-c', dest='clean'),
    SetTrueOption('-i', dest='ios'),
    SetTrueOption('-l', dest='build_lldb'),
    SetTrueOption('-n', dest='dry_run'),
    SetTrueOption('-p', dest='build_swiftpm'),

    SetFalseOption('--no-legacy-impl', dest='legacy_impl'),

    EnableOption('--android'),
    EnableOption('--build-external-benchmarks'),
    EnableOption('--build-ninja'),
    EnableOption('--build-runtime-with-host-compiler'),
    EnableOption('--build-swift-dynamic-sdk-overlay'),
    EnableOption('--build-swift-dynamic-stdlib'),
    EnableOption('--build-swift-static-sdk-overlay'),
    EnableOption('--build-swift-static-stdlib'),
    EnableOption('--build-swift-stdlib-unittest-extra'),
    EnableOption('--distcc'),
    EnableOption('--enable-asan'),
    EnableOption('--enable-lsan'),
    EnableOption('--enable-sanitize-coverage'),
    EnableOption('--enable-tsan'),
    EnableOption('--enable-tsan-runtime'),
    EnableOption('--enable-ubsan'),
    EnableOption('--export-compile-commands'),
    EnableOption('--foundation', dest='build_foundation'),
    EnableOption('--host-test'),
    EnableOption('--libdispatch', dest='build_libdispatch'),
    EnableOption('--libicu', dest='build_libicu'),
    EnableOption('--long-test'),
    EnableOption('--show-sdks'),
    EnableOption('--stress-test'),
    EnableOption('--test'),
    EnableOption('--test-optimize-for-size'),
    EnableOption('--test-optimized'),
    EnableOption('--tvos'),
    EnableOption('--validation-test'),
    EnableOption('--verbose-build'),
    EnableOption('--watchos'),
    EnableOption('--xctest', dest='build_xctest'),

    DisableOption('--skip-build-android', dest='build_android'),
    DisableOption('--skip-build-benchmarks', dest='build_benchmarks'),
    DisableOption('--skip-build-cygwin', dest='build_cygwin'),
    DisableOption('--skip-build-freebsd', dest='build_freebsd'),
    DisableOption('--skip-build-ios', dest='build_ios'),
    DisableOption('--skip-build-ios-device', dest='build_ios_device'),
    DisableOption('--skip-build-ios-simulator',
                  dest='build_ios_simulator'),
    DisableOption('--skip-build-linux', dest='build_linux'),
    DisableOption('--skip-build-osx', dest='build_osx'),
    DisableOption('--skip-build-tvos', dest='build_tvos'),
    DisableOption('--skip-build-tvos-device', dest='build_tvos_device'),
    DisableOption('--skip-build-tvos-simulator',
                  dest='build_tvos_simulator'),
    DisableOption('--skip-build-watchos', dest='build_watchos'),
    DisableOption('--skip-build-watchos-device',
                  dest='build_watchos_device'),
    DisableOption('--skip-build-watchos-simulator',
                  dest='build_watchos_simulator'),
    DisableOption('--skip-test-android-host', dest='test_android_host'),
    DisableOption('--skip-test-cygwin', dest='test_cygwin'),
    DisableOption('--skip-test-freebsd', dest='test_freebsd'),
    DisableOption('--skip-test-ios', dest='test_ios'),
    DisableOption('--skip-test-ios-32bit-simulator',
                  dest='test_ios_32bit_simulator'),
    DisableOption('--skip-test-ios-host', dest='test_ios_host'),
    DisableOption('--skip-test-ios-simulator', dest='test_ios_simulator'),
    DisableOption('--skip-test-linux', dest='test_linux'),
    DisableOption('--skip-test-osx', dest='test_osx'),
    DisableOption('--skip-test-tvos', dest='test_tvos'),
    DisableOption('--skip-test-tvos-host', dest='test_tvos_host'),
    DisableOption('--skip-test-tvos-simulator',
                  dest='test_tvos_simulator'),
    DisableOption('--skip-test-watchos', dest='test_watchos'),
    DisableOption('--skip-test-watchos-host', dest='test_watchos_host'),
    DisableOption('--skip-test-watchos-simulator',
                  dest='test_watchos_simulator'),

    ChoicesOption('--android-ndk-gcc-version',
                  choices=['4.8', '4.9']),
    ChoicesOption('--compiler-vendor',
                  choices=['none', 'apple']),
    ChoicesOption('--swift-analyze-code-coverage',
                  choices=['false', 'not-merged', 'merged']),

    StrOption('--android-api-level'),
    StrOption('--build-args'),
    StrOption('--build-stdlib-deployment-targets'),
    StrOption('--darwin-deployment-version-ios'),
    StrOption('--darwin-deployment-version-osx'),
    StrOption('--darwin-deployment-version-tvos'),
    StrOption('--darwin-deployment-version-watchos'),
    StrOption('--darwin-xcrun-toolchain'),
    StrOption('--host-target'),
    StrOption('--lit-args'),
    StrOption('--llvm-targets-to-build'),

    PathOption('--android-deploy-device-path'),
    PathOption('--android-icu-i18n'),
    PathOption('--android-icu-i18n-include'),
    PathOption('--android-icu-uc'),
    PathOption('--android-icu-uc-include'),
    PathOption('--android-ndk'),
    PathOption('--build-subdir'),
    PathOption('--clang-profile-instr-use'),
    PathOption('--cmake'),
    PathOption('--coverage-db'),
    PathOption('--host-cc'),
    PathOption('--host-cxx'),
    PathOption('--host-libtool'),
    PathOption('--host-lipo'),
    PathOption('--install-prefix'),
    PathOption('--install-symroot'),
    PathOption('--symbols-package'),

    IntOption('--benchmark-num-o-iterations'),
    IntOption('--benchmark-num-onone-iterations'),
    IntOption('--jobs', dest='build_jobs'),
    IntOption('--llvm-max-parallel-lto-link-jobs'),
    IntOption('--swift-tools-max-parallel-lto-link-jobs'),
    IntOption('-j', dest='build_jobs'),

    AppendOption('--cross-compile-hosts'),
    AppendOption('--extra-cmake-options'),
    AppendOption('--extra-swift-args'),
    AppendOption('--stdlib-deployment-targets'),
    AppendOption('--test-paths'),

    UnsupportedOption('--build-jobs'),
    UnsupportedOption('--common-cmake-options'),
    UnsupportedOption('--only-execute'),
    UnsupportedOption('--skip-test-optimize-for-size'),
    UnsupportedOption('--skip-test-optimized'),

    # NOTE: LTO flag is a special case that acts both as an option and has
    # valid choices
    SetOption('--lto', dest='lto_type'),
    ChoicesOption('--lto', dest='lto_type', choices=['thin', 'full']),

    # NOTE: We'll need to manually test the behavior of these since they
    # validate compiler version strings.
    IgnoreOption('--clang-compiler-version'),
    IgnoreOption('--clang-user-visible-version'),
    IgnoreOption('--swift-compiler-version'),
    IgnoreOption('--swift-user-visible-version'),

    # TODO: Migrate to unavailable options once new parser is in place
    IgnoreOption('-I'),
    IgnoreOption('--ios-all'),
    IgnoreOption('--tvos-all'),
    IgnoreOption('--watchos-all'),
]
