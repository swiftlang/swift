# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


import multiprocessing

import android.adb.commands

from swift_build_support.swift_build_support import arguments
from swift_build_support.swift_build_support import host
from swift_build_support.swift_build_support import targets
from swift_build_support.swift_build_support.targets import \
    StdlibDeploymentTarget

from . import argparse
from . import defaults


__all__ = [
    'create_argument_parser',
]


class _ApplyDefaultsArgumentParser(argparse.ArgumentParser):
    """Wrapper class around the default ArgumentParser that allows for
    post-processing the parsed argument namespace to apply default argument
    transformations.
    """

    def __init__(self, apply_defaults=None, *args, **kwargs):
        self._apply_defaults = apply_defaults
        super(_ApplyDefaultsArgumentParser, self).__init__(*args, **kwargs)

    def parse_known_args(self, args=None, namespace=None):
        args, argv = super(_ApplyDefaultsArgumentParser, self)\
            .parse_known_args(args, namespace)

        self._apply_defaults(args)
        return args, argv


def _apply_default_arguments(args):
    """Preprocess argument namespace to apply default behaviors.
    """

    # Build cmark if any cmark-related options were specified.
    if (args.cmark_build_variant is not None):
        args.build_cmark = True

    # Build LLDB if any LLDB-related options were specified.
    if args.lldb_build_variant is not None or \
       args.lldb_assertions is not None:
        args.build_lldb = True

    # Set the default build variant.
    if args.build_variant is None:
        args.build_variant = 'Debug'

    if args.llvm_build_variant is None:
        args.llvm_build_variant = args.build_variant

    if args.swift_build_variant is None:
        args.swift_build_variant = args.build_variant

    if args.swift_stdlib_build_variant is None:
        args.swift_stdlib_build_variant = args.build_variant

    if args.cmark_build_variant is None:
        args.cmark_build_variant = args.swift_build_variant

    if args.lldb_build_variant is None:
        args.lldb_build_variant = args.build_variant

    if args.foundation_build_variant is None:
        args.foundation_build_variant = args.build_variant

    if args.libdispatch_build_variant is None:
        args.libdispatch_build_variant = args.build_variant

    if args.libicu_build_variant is None:
        args.libicu_build_variant = args.build_variant

    # Assertions are enabled by default.
    if args.assertions is None:
        args.assertions = True

    # Propagate the default assertions setting.
    if args.cmark_assertions is None:
        args.cmark_assertions = args.assertions

    if args.llvm_assertions is None:
        args.llvm_assertions = args.assertions

    if args.swift_assertions is None:
        args.swift_assertions = args.assertions

    if args.swift_stdlib_assertions is None:
        args.swift_stdlib_assertions = args.assertions

    # Set the default CMake generator.
    if args.cmake_generator is None:
        args.cmake_generator = 'Ninja'

    # --ios-all etc are not supported by open-source Swift.
    if args.ios_all:
        raise ValueError('error: --ios-all is unavailable in open-source '
                         'Swift.\nUse --ios to skip iOS device tests.')

    if args.tvos_all:
        raise ValueError('error: --tvos-all is unavailable in open-source '
                         'Swift.\nUse --tvos to skip tvOS device tests.')

    if args.watchos_all:
        raise ValueError('error: --watchos-all is unavailable in open-source '
                         'Swift.\nUse --watchos to skip watchOS device tests.')

    # Propagate global --skip-build
    if args.skip_build:
        args.build_linux = False
        args.build_freebsd = False
        args.build_cygwin = False
        args.build_osx = False
        args.build_ios = False
        args.build_tvos = False
        args.build_watchos = False
        args.build_android = False
        args.build_benchmarks = False
        args.build_external_benchmarks = False
        args.build_lldb = False
        args.build_llbuild = False
        args.build_swiftpm = False
        args.build_xctest = False
        args.build_foundation = False
        args.build_libdispatch = False
        args.build_libicu = False
        args.build_playgroundsupport = False

    # --skip-{ios,tvos,watchos} or --skip-build-{ios,tvos,watchos} are
    # merely shorthands for --skip-build-{**os}-{device,simulator}
    if not args.ios or not args.build_ios:
        args.build_ios_device = False
        args.build_ios_simulator = False

    if not args.tvos or not args.build_tvos:
        args.build_tvos_device = False
        args.build_tvos_simulator = False

    if not args.watchos or not args.build_watchos:
        args.build_watchos_device = False
        args.build_watchos_simulator = False

    if not args.android or not args.build_android:
        args.build_android = False

    # --validation-test implies --test.
    if args.validation_test:
        args.test = True

    # --test-optimized implies --test.
    if args.test_optimized:
        args.test = True

    # --test-optimize-size implies --test.
    if args.test_optimize_for_size:
        args.test = True

    # If none of tests specified skip swift stdlib test on all platforms
    if not args.test and not args.validation_test and not args.long_test:
        args.test_linux = False
        args.test_freebsd = False
        args.test_cygwin = False
        args.test_osx = False
        args.test_ios = False
        args.test_tvos = False
        args.test_watchos = False

    # --skip-test-ios is merely a shorthand for host and simulator tests.
    if not args.test_ios:
        args.test_ios_host = False
        args.test_ios_simulator = False
    # --skip-test-tvos is merely a shorthand for host and simulator tests.
    if not args.test_tvos:
        args.test_tvos_host = False
        args.test_tvos_simulator = False
    # --skip-test-watchos is merely a shorthand for host and simulator
    # --tests.
    if not args.test_watchos:
        args.test_watchos_host = False
        args.test_watchos_simulator = False

    # --skip-build-{ios,tvos,watchos}-{device,simulator} implies
    # --skip-test-{ios,tvos,watchos}-{host,simulator}
    if not args.build_ios_device:
        args.test_ios_host = False
    if not args.build_ios_simulator:
        args.test_ios_simulator = False

    if not args.build_tvos_device:
        args.test_tvos_host = False
    if not args.build_tvos_simulator:
        args.test_tvos_simulator = False

    if not args.build_watchos_device:
        args.test_watchos_host = False
    if not args.build_watchos_simulator:
        args.test_watchos_simulator = False

    if not args.build_android:
        args.test_android_host = False

    if not args.host_test:
        args.test_ios_host = False
        args.test_tvos_host = False
        args.test_watchos_host = False
        args.test_android_host = False


def create_argument_parser():
    """Return a configured argument parser."""

    # NOTE: USAGE, DESCRIPTION and EPILOG are defined at the bottom of the file
    parser = _ApplyDefaultsArgumentParser(
        apply_defaults=_apply_default_arguments,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        usage=USAGE,
        description=DESCRIPTION,
        epilog=EPILOG)

    builder = parser.to_builder()

    # Prepare DSL functions
    option = builder.add_option
    set_defaults = builder.set_defaults
    in_group = builder.in_group
    mutually_exclusive_group = builder.mutually_exclusive_group

    # Prepare DSL actions
    store = builder.actions.store
    store_path = builder.actions.store_path

    # -------------------------------------------------------------------------
    # Top-level options

    parser.add_argument(
        '-n', '--dry-run',
        action='store_true',
        default=False,
        help='print the commands that would be executed, but do not execute '
             'them')
    parser.add_argument(
        '--no-legacy-impl',
        action='store_false',
        dest='legacy_impl',
        default=True,
        help='avoid legacy implementation')

    parser.add_argument(
        '--build-runtime-with-host-compiler',
        action=arguments.action.enable,
        help='Use the host compiler, not the self-built one to compile the '
             'Swift runtime')

    parser.add_argument(
        '-i', '--ios',
        action='store_true',
        help='also build for iOS, but disallow tests that require an iOS '
             'device')
    parser.add_argument(
        '-I', '--ios-all',
        action='store_true',
        dest='ios_all',
        help='also build for iOS, and allow all iOS tests')
    parser.add_argument(
        '--skip-ios',
        action='store_false',
        dest='ios',
        help='set to skip everything iOS-related')

    parser.add_argument(
        '--tvos',
        action=arguments.action.enable,
        help='also build for tvOS, but disallow tests that require a tvos '
             'device')
    parser.add_argument(
        '--tvos-all',
        action=arguments.action.enable,
        dest='tvos_all',
        help='also build for tvOS, and allow all tvOS tests')
    parser.add_argument(
        '--skip-tvos',
        action='store_false',
        dest='tvos',
        help='set to skip everything tvOS-related')

    parser.add_argument(
        '--watchos',
        action=arguments.action.enable,
        help='also build for watchOS, but disallow tests that require an '
             'watchOS device')
    parser.add_argument(
        '--watchos-all',
        action=arguments.action.enable,
        dest='watchos_all',
        help='also build for Apple watchOS, and allow all Apple watchOS tests')
    parser.add_argument(
        '--skip-watchos',
        action='store_false',
        dest='watchos',
        help='set to skip everything watchOS-related')

    parser.add_argument(
        '--android',
        action=arguments.action.enable,
        help='also build for Android')

    parser.add_argument(
        '--swift-analyze-code-coverage',
        dest='swift_analyze_code_coverage',
        choices=['false', 'not-merged', 'merged'],
        # so CMake can see the inert mode as a false value
        default=defaults.SWIFT_ANALYZE_CODE_COVERAGE,
        help='enable code coverage analysis in Swift (false, not-merged, '
             'merged).')

    parser.add_argument(
        '--build-subdir',
        metavar='PATH',
        help='name of the directory under $SWIFT_BUILD_ROOT where the build '
             'products will be placed')
    parser.add_argument(
        '--install-prefix',
        default=targets.install_prefix(),
        metavar='PATH',
        help='The installation prefix. This is where built Swift products '
             '(like bin, lib, and include) will be installed.')
    parser.add_argument(
        '--install-symroot',
        metavar='PATH',
        help='the path to install debug symbols into')

    parser.add_argument(
        '-j', '--jobs',
        type=int,
        dest='build_jobs',
        default=multiprocessing.cpu_count(),
        help='the number of parallel build jobs to use')

    parser.add_argument(
        '--darwin-xcrun-toolchain',
        default=defaults.DARWIN_XCRUN_TOOLCHAIN,
        help='the name of the toolchain to use on Darwin')
    parser.add_argument(
        '--cmake',
        type=arguments.type.executable,
        metavar='PATH',
        help='the path to a CMake executable that will be used to build '
             'Swift')
    parser.add_argument(
        '--show-sdks',
        action=arguments.action.enable,
        help='print installed Xcode and SDK versions')

    parser.add_argument(
        '--extra-swift-args',
        action='append',
        dest='extra_swift_args',
        default=[],
        help='Pass through extra flags to swift in the form of a cmake list '
             '"module_regexp;flag". Can be called multiple times to add '
             'multiple such module_regexp flag pairs. All semicolons in flags '
             'must be escaped with a "\\"')

    parser.add_argument(
        '--host-cc',
        type=arguments.type.executable,
        metavar='PATH',
        help='the absolute path to CC, the "clang" compiler for the host '
             'platform. Default is auto detected.')
    parser.add_argument(
        '--host-cxx',
        type=arguments.type.executable,
        metavar='PATH',
        help='the absolute path to CXX, the "clang++" compiler for the host '
             'platform. Default is auto detected.')
    parser.add_argument(
        '--host-lipo',
        type=arguments.type.executable,
        metavar='PATH',
        help='the absolute path to lipo. Default is auto detected.')
    parser.add_argument(
        '--host-libtool',
        type=arguments.type.executable,
        metavar='PATH',
        help='the absolute path to libtool. Default is auto detected.')
    parser.add_argument(
        '--distcc',
        action=arguments.action.enable,
        help='use distcc in pump mode')
    parser.add_argument(
        '--enable-asan',
        action=arguments.action.enable,
        help='enable Address Sanitizer')
    parser.add_argument(
        '--enable-ubsan',
        action=arguments.action.enable,
        help='enable Undefined Behavior Sanitizer')
    parser.add_argument(
        '--enable-tsan',
        action=arguments.action.enable,
        help='enable Thread Sanitizer for swift tools')
    parser.add_argument(
        '--enable-tsan-runtime',
        action=arguments.action.enable,
        help='enable Thread Sanitizer on the swift runtime')
    parser.add_argument(
        '--enable-lsan',
        action=arguments.action.enable,
        help='enable Leak Sanitizer for swift tools')

    parser.add_argument(
        '--compiler-vendor',
        choices=['none', 'apple'],
        default=defaults.COMPILER_VENDOR,
        help='Compiler vendor name')
    parser.add_argument(
        '--clang-compiler-version',
        type=arguments.type.clang_compiler_version,
        metavar='MAJOR.MINOR.PATCH',
        help='string that indicates a compiler version for Clang')
    parser.add_argument(
        '--clang-user-visible-version',
        type=arguments.type.clang_compiler_version,
        default=defaults.CLANG_USER_VISIBLE_VERSION,
        metavar='MAJOR.MINOR.PATCH',
        help='User-visible version of the embedded Clang and LLVM compilers')
    parser.add_argument(
        '--swift-compiler-version',
        type=arguments.type.swift_compiler_version,
        metavar='MAJOR.MINOR',
        help='string that indicates a compiler version for Swift')
    parser.add_argument(
        '--swift-user-visible-version',
        type=arguments.type.swift_compiler_version,
        default=defaults.SWIFT_USER_VISIBLE_VERSION,
        metavar='MAJOR.MINOR',
        help='User-visible version of the embedded Swift compiler')

    parser.add_argument(
        '--darwin-deployment-version-osx',
        default=defaults.DARWIN_DEPLOYMENT_VERSION_OSX,
        metavar='MAJOR.MINOR',
        help='minimum deployment target version for OS X')
    parser.add_argument(
        '--darwin-deployment-version-ios',
        default=defaults.DARWIN_DEPLOYMENT_VERSION_IOS,
        metavar='MAJOR.MINOR',
        help='minimum deployment target version for iOS')
    parser.add_argument(
        '--darwin-deployment-version-tvos',
        default=defaults.DARWIN_DEPLOYMENT_VERSION_TVOS,
        metavar='MAJOR.MINOR',
        help='minimum deployment target version for tvOS')
    parser.add_argument(
        '--darwin-deployment-version-watchos',
        default=defaults.DARWIN_DEPLOYMENT_VERSION_WATCHOS,
        metavar='MAJOR.MINOR',
        help='minimum deployment target version for watchOS')

    parser.add_argument(
        '--extra-cmake-options',
        action=arguments.action.concat,
        type=arguments.type.shell_split,
        default=[],
        help='Pass through extra options to CMake in the form of comma '
             'separated options "-DCMAKE_VAR1=YES,-DCMAKE_VAR2=/tmp". Can be '
             'called multiple times to add multiple such options.')

    parser.add_argument(
        '--build-args',
        type=arguments.type.shell_split,
        default=[],
        help='arguments to the build tool. This would be prepended to the '
             'default argument that is "-j8" when CMake generator is '
             '"Ninja".')

    parser.add_argument(
        '--verbose-build',
        action=arguments.action.enable,
        help='print the commands executed during the build')

    parser.add_argument(
        '--lto',
        dest='lto_type',
        nargs='?',
        choices=['thin', 'full'],
        const='full',
        default=None,
        metavar='LTO_TYPE',
        help='use lto optimization on llvm/swift tools. This does not '
             'imply using lto on the swift standard library or runtime. '
             'Options: thin, full. If no optional arg is provided, full is '
             'chosen by default')

    parser.add_argument(
        '--clang-profile-instr-use',
        metavar='PATH',
        help='profile file to use for clang PGO')

    default_max_lto_link_job_counts = host.max_lto_link_job_counts()
    parser.add_argument(
        '--llvm-max-parallel-lto-link-jobs',
        default=default_max_lto_link_job_counts['llvm'],
        metavar='COUNT',
        help='the maximum number of parallel link jobs to use when compiling '
             'llvm')

    parser.add_argument(
        '--swift-tools-max-parallel-lto-link-jobs',
        default=default_max_lto_link_job_counts['swift'],
        metavar='COUNT',
        help='the maximum number of parallel link jobs to use when compiling '
             'swift tools.')

    parser.add_argument(
        '--enable-sil-ownership',
        action='store_true',
        help='Enable the SIL ownership model')

    parser.add_argument(
        '--force-optimized-typechecker',
        action='store_true',
        help='Force the type checker to be built with '
             'optimization')

    parser.add_argument(
        '--lit-args',
        default='-sv',
        metavar='LITARGS',
        help='lit args to use when testing')

    parser.add_argument(
        '--coverage-db',
        metavar='PATH',
        help='coverage database to use when prioritizing testing')

    parser.add_argument(
        # Explicitly unavailable options here.
        '--build-jobs',
        '--common-cmake-options',
        '--only-execute',
        '--skip-test-optimize-for-size',
        '--skip-test-optimized',
        action=arguments.action.unavailable)

    # -------------------------------------------------------------------------
    targets_group = parser.add_argument_group(
        title='Host and cross-compilation targets')
    targets_group.add_argument(
        '--host-target',
        default=StdlibDeploymentTarget.host_target().name,
        help='The host target. LLVM, Clang, and Swift will be built for this '
             'target. The built LLVM and Clang will be used to compile Swift '
             'for the cross-compilation targets.')
    targets_group.add_argument(
        '--cross-compile-hosts',
        action=arguments.action.concat,
        type=arguments.type.shell_split,
        default=[],
        help='A space separated list of targets to cross-compile host Swift '
             'tools for. Can be used multiple times.')
    targets_group.add_argument(
        '--stdlib-deployment-targets',
        action=arguments.action.concat,
        type=arguments.type.shell_split,
        default=None,
        help='list of targets to compile or cross-compile the Swift standard '
             'library for. %(default)s by default.')
    targets_group.add_argument(
        '--build-stdlib-deployment-targets',
        type=arguments.type.shell_split,
        default=['all'],
        help='A space-separated list that filters which of the configured '
             'targets to build the Swift standard library for, or "all".')

    # -------------------------------------------------------------------------
    projects_group = parser.add_argument_group(
        title='Options to select projects')
    projects_group.add_argument(
        '-l', '--lldb',
        action='store_true',
        dest='build_lldb',
        help='build LLDB')
    projects_group.add_argument(
        '-b', '--llbuild',
        action='store_true',
        dest='build_llbuild',
        help='build llbuild')
    projects_group.add_argument(
        '-p', '--swiftpm',
        action='store_true',
        dest='build_swiftpm',
        help='build swiftpm')
    projects_group.add_argument(
        '--xctest',
        action=arguments.action.enable,
        dest='build_xctest',
        help='build xctest')
    projects_group.add_argument(
        '--foundation',
        action=arguments.action.enable,
        dest='build_foundation',
        help='build foundation')
    projects_group.add_argument(
        '--libdispatch',
        action=arguments.action.enable,
        dest='build_libdispatch',
        help='build libdispatch')
    projects_group.add_argument(
        '--libicu',
        action=arguments.action.enable,
        dest='build_libicu',
        help='build libicu')
    projects_group.add_argument(
        '--playgroundsupport',
        action='store_true',
        dest='build_playgroundsupport',
        help='build PlaygroundSupport')
    projects_group.add_argument(
        '--build-ninja',
        action=arguments.action.enable,
        help='build the Ninja tool')

    # -------------------------------------------------------------------------
    extra_actions_group = parser.add_argument_group(
        title='Extra actions to perform before or in addition to building')
    extra_actions_group.add_argument(
        '-c', '--clean',
        action='store_true',
        help='do a clean build')
    extra_actions_group.add_argument(
        '--export-compile-commands',
        action=arguments.action.enable,
        help='generate compilation databases in addition to building')
    extra_actions_group.add_argument(
        '--symbols-package',
        metavar='PATH',
        help='if provided, an archive of the symbols directory will be '
             'generated at this path')

    # -------------------------------------------------------------------------
    build_variant_group = parser.add_mutually_exclusive_group(required=False)
    build_variant_group.add_argument(
        '-d', '--debug',
        action='store_const',
        const='Debug',
        dest='build_variant',
        help='build the Debug variant of everything (LLVM, Clang, Swift host '
             'tools, target Swift standard libraries, LLDB (if enabled) '
             '(default)')
    build_variant_group.add_argument(
        '-r', '--release-debuginfo',
        action='store_const',
        const='RelWithDebInfo',
        dest='build_variant',
        help='build the RelWithDebInfo variant of everything (default is '
             'Debug)')
    build_variant_group.add_argument(
        '-R', '--release',
        action='store_const',
        const='Release',
        dest='build_variant',
        help='build the Release variant of everything (default is Debug)')

    # -------------------------------------------------------------------------
    build_variant_override_group = parser.add_argument_group(
        title='Override build variant for a specific project')
    build_variant_override_group.add_argument(
        '--debug-llvm',
        action='store_const',
        const='Debug',
        dest='llvm_build_variant',
        help='build the Debug variant of LLVM')
    build_variant_override_group.add_argument(
        '--debug-swift',
        action='store_const',
        const='Debug',
        dest='swift_build_variant',
        help='build the Debug variant of Swift host tools')
    build_variant_override_group.add_argument(
        '--debug-swift-stdlib',
        action='store_const',
        const='Debug',
        dest='swift_stdlib_build_variant',
        help='build the Debug variant of the Swift standard library and SDK '
             'overlay')
    build_variant_override_group.add_argument(
        '--debug-lldb',
        action='store_const',
        const='Debug',
        dest='lldb_build_variant',
        help='build the Debug variant of LLDB')
    build_variant_override_group.add_argument(
        '--debug-cmark',
        action='store_const',
        const='Debug',
        dest='cmark_build_variant',
        help='build the Debug variant of CommonMark')
    build_variant_override_group.add_argument(
        '--debug-foundation',
        action='store_const',
        const='Debug',
        dest='foundation_build_variant',
        help='build the Debug variant of Foundation')
    build_variant_override_group.add_argument(
        '--debug-libdispatch',
        action='store_const',
        const='Debug',
        dest='libdispatch_build_variant',
        help='build the Debug variant of libdispatch')
    build_variant_override_group.add_argument(
        '--debug-libicu',
        action='store_const',
        const='Debug',
        dest='libicu_build_variant',
        help='build the Debug variant of libicu')

    # -------------------------------------------------------------------------
    # Assertions group

    with mutually_exclusive_group():
        set_defaults(assertions=True)

        # TODO: Convert to store_true
        option('--assertions', store,
               const=True,
               help='enable assertions in all projects')

        # TODO: Convert to store_false
        option('--no-assertions', store('assertions'),
               const=False,
               help='disable assertions in all projects')

    # -------------------------------------------------------------------------
    in_group('Control assertions in a specific project')

    option('--cmark-assertions', store,
           const=True,
           help='enable assertions in CommonMark')

    option('--llvm-assertions', store,
           const=True,
           help='enable assertions in LLVM')
    option('--no-llvm-assertions', store('llvm_assertions'),
           const=False,
           help='disable assertions in LLVM')

    option('--swift-assertions', store,
           const=True,
           help='enable assertions in Swift')
    option('--no-swift-assertions', store('swift_assertions'),
           const=False,
           help='disable assertions in Swift')

    option('--swift-stdlib-assertions', store,
           const=True,
           help='enable assertions in the Swift standard library')
    option('--no-swift-stdlib-assertions', store('swift_stdlib_assertions'),
           const=False,
           help='disable assertions in the Swift standard library')

    option('--lldb-assertions', store,
           const=True,
           help='enable assertions in LLDB')
    option('--no-lldb-assertions', store('lldb_assertions'),
           const=False,
           help='disable assertions in LLDB')

    # -------------------------------------------------------------------------
    in_group('Select the CMake generator')

    set_defaults(cmake_generator=defaults.CMAKE_GENERATOR)

    option(['-e', '--eclipse'], store('cmake_generator'),
           const='Eclipse CDT4 - Ninja',
           help="use CMake's Eclipse generator (%(default)s by default)")
    option(['-m', '--make'], store('cmake_generator'),
           const='Unix Makefiles',
           help="use CMake's Makefile generator (%(default)s by default)")
    option(['-x', '--xcode'], store('cmake_generator'),
           const='Xcode',
           help="use CMake's Xcode generator (%(default)s by default)")

    # -------------------------------------------------------------------------
    run_tests_group = parser.add_argument_group(
        title='Run tests')

    # NOTE: We can't merge -t and --test, because nargs='?' makes
    #       `-ti` to be treated as `-t=i`.
    run_tests_group.add_argument(
        '-t',
        action='store_const',
        const=True,
        dest='test',
        help='test Swift after building')
    run_tests_group.add_argument(
        '--test',
        action=arguments.action.enable,
        help='test Swift after building')
    run_tests_group.add_argument(
        '-T',
        action='store_const',
        const=True,
        dest='validation_test',
        help='run the validation test suite (implies --test)')
    run_tests_group.add_argument(
        '--validation-test',
        action=arguments.action.enable,
        help='run the validation test suite (implies --test)')
    run_tests_group.add_argument(
        '--test-paths',
        action=arguments.action.concat,
        type=arguments.type.shell_split,
        default=[],
        help='run tests located in specific directories and/or files '
             '(implies --test and/or --validation-test)')
    run_tests_group.add_argument(
        '-o',
        action='store_const',
        const=True,
        dest='test_optimized',
        help='run the test suite in optimized mode too (implies --test)')
    run_tests_group.add_argument(
        '--test-optimized',
        action=arguments.action.enable,
        help='run the test suite in optimized mode too (implies --test)')
    run_tests_group.add_argument(
        '-s',
        action='store_const',
        const=True,
        dest='test_optimize_for_size',
        help='run the test suite in optimize for size mode too '
             '(implies --test)')
    run_tests_group.add_argument(
        '--test-optimize-for-size',
        action=arguments.action.enable,
        help='run the test suite in optimize for size mode too '
             '(implies --test)')
    run_tests_group.add_argument(
        '--long-test',
        action=arguments.action.enable,
        help='run the long test suite')
    run_tests_group.add_argument(
        '--host-test',
        action=arguments.action.enable,
        help='run executable tests on host devices (such as iOS or tvOS)')
    run_tests_group.add_argument(
        '-B', '--benchmark',
        action='store_true',
        help='run the Swift Benchmark Suite after building')
    run_tests_group.add_argument(
        '--benchmark-num-o-iterations',
        type=int,
        default=3,
        metavar='N',
        help='if the Swift Benchmark Suite is run after building, run N '
             'iterations with -O')
    run_tests_group.add_argument(
        '--benchmark-num-onone-iterations',
        type=int,
        default=3,
        metavar='N',
        help='if the Swift Benchmark Suite is run after building, run N '
             'iterations with -Onone')
    run_tests_group.add_argument(
        '--skip-test-osx',
        action=arguments.action.disable,
        dest='test_osx',
        help='skip testing Swift stdlibs for Mac OS X')
    run_tests_group.add_argument(
        '--skip-test-linux',
        action=arguments.action.disable,
        dest='test_linux',
        help='skip testing Swift stdlibs for Linux')
    run_tests_group.add_argument(
        '--skip-test-freebsd',
        action=arguments.action.disable,
        dest='test_freebsd',
        help='skip testing Swift stdlibs for FreeBSD')
    run_tests_group.add_argument(
        '--skip-test-cygwin',
        action=arguments.action.disable,
        dest='test_cygwin',
        help='skip testing Swift stdlibs for Cygwin')

    # -------------------------------------------------------------------------
    run_build_group = parser.add_argument_group(
        title='Run build')
    run_build_group.add_argument(
        '--build-swift-dynamic-stdlib',
        action=arguments.action.enable,
        default=True,
        help='build dynamic variants of the Swift standard library')
    run_build_group.add_argument(
        '--build-swift-static-stdlib',
        action=arguments.action.enable,
        help='build static variants of the Swift standard library')
    run_build_group.add_argument(
        '--build-swift-dynamic-sdk-overlay',
        action=arguments.action.enable,
        default=True,
        help='build dynamic variants of the Swift SDK overlay')
    run_build_group.add_argument(
        '--build-swift-static-sdk-overlay',
        action=arguments.action.enable,
        help='build static variants of the Swift SDK overlay')
    run_build_group.add_argument(
        '--build-swift-stdlib-unittest-extra',
        action=arguments.action.enable,
        help='Build optional StdlibUnittest components')
    run_build_group.add_argument(
        '-S', '--skip-build',
        action='store_true',
        help='generate build directory only without building')
    run_build_group.add_argument(
        '--skip-build-linux',
        action=arguments.action.disable,
        dest='build_linux',
        help='skip building Swift stdlibs for Linux')
    run_build_group.add_argument(
        '--skip-build-freebsd',
        action=arguments.action.disable,
        dest='build_freebsd',
        help='skip building Swift stdlibs for FreeBSD')
    run_build_group.add_argument(
        '--skip-build-cygwin',
        action=arguments.action.disable,
        dest='build_cygwin',
        help='skip building Swift stdlibs for Cygwin')
    run_build_group.add_argument(
        '--skip-build-osx',
        action=arguments.action.disable,
        dest='build_osx',
        help='skip building Swift stdlibs for MacOSX')

    run_build_group.add_argument(
        '--skip-build-ios',
        action=arguments.action.disable,
        dest='build_ios',
        help='skip building Swift stdlibs for iOS')
    run_build_group.add_argument(
        '--skip-build-ios-device',
        action=arguments.action.disable,
        dest='build_ios_device',
        help='skip building Swift stdlibs for iOS devices '
             '(i.e. build simulators only)')
    run_build_group.add_argument(
        '--skip-build-ios-simulator',
        action=arguments.action.disable,
        dest='build_ios_simulator',
        help='skip building Swift stdlibs for iOS simulator '
             '(i.e. build devices only)')

    run_build_group.add_argument(
        '--skip-build-tvos',
        action=arguments.action.disable,
        dest='build_tvos',
        help='skip building Swift stdlibs for tvOS')
    run_build_group.add_argument(
        '--skip-build-tvos-device',
        action=arguments.action.disable,
        dest='build_tvos_device',
        help='skip building Swift stdlibs for tvOS devices '
             '(i.e. build simulators only)')
    run_build_group.add_argument(
        '--skip-build-tvos-simulator',
        action=arguments.action.disable,
        dest='build_tvos_simulator',
        help='skip building Swift stdlibs for tvOS simulator '
             '(i.e. build devices only)')

    run_build_group.add_argument(
        '--skip-build-watchos',
        action=arguments.action.disable,
        dest='build_watchos',
        help='skip building Swift stdlibs for watchOS')
    run_build_group.add_argument(
        '--skip-build-watchos-device',
        action=arguments.action.disable,
        dest='build_watchos_device',
        help='skip building Swift stdlibs for watchOS devices '
             '(i.e. build simulators only)')
    run_build_group.add_argument(
        '--skip-build-watchos-simulator',
        action=arguments.action.disable,
        dest='build_watchos_simulator',
        help='skip building Swift stdlibs for watchOS simulator '
             '(i.e. build devices only)')

    run_build_group.add_argument(
        '--skip-build-android',
        action=arguments.action.disable,
        dest='build_android',
        help='skip building Swift stdlibs for Android')

    run_build_group.add_argument(
        '--skip-build-benchmarks',
        action=arguments.action.disable,
        dest='build_benchmarks',
        help='skip building Swift Benchmark Suite')

    run_build_group.add_argument(
        '--build-external-benchmarks',
        action=arguments.action.enable,
        dest='build_external_benchmarks',
        help='skip building Swift Benchmark Suite')

    # -------------------------------------------------------------------------
    skip_test_group = parser.add_argument_group(
        title='Skip testing specified targets')
    skip_test_group.add_argument(
        '--skip-test-ios',
        action=arguments.action.disable,
        dest='test_ios',
        help='skip testing all iOS targets. Equivalent to specifying both '
             '--skip-test-ios-simulator and --skip-test-ios-host')
    skip_test_group.add_argument(
        '--skip-test-ios-simulator',
        action=arguments.action.disable,
        dest='test_ios_simulator',
        help='skip testing iOS simulator targets')
    skip_test_group.add_argument(
        '--skip-test-ios-32bit-simulator',
        action=arguments.action.disable,
        dest='test_ios_32bit_simulator',
        help='skip testing iOS 32 bit simulator targets')
    skip_test_group.add_argument(
        '--skip-test-ios-host',
        action=arguments.action.disable,
        dest='test_ios_host',
        help='skip testing iOS device targets on the host machine (the phone '
             'itself)')
    skip_test_group.add_argument(
        '--skip-test-tvos',
        action=arguments.action.disable,
        dest='test_tvos',
        help='skip testing all tvOS targets. Equivalent to specifying both '
             '--skip-test-tvos-simulator and --skip-test-tvos-host')
    skip_test_group.add_argument(
        '--skip-test-tvos-simulator',
        action=arguments.action.disable,
        dest='test_tvos_simulator',
        help='skip testing tvOS simulator targets')
    skip_test_group.add_argument(
        '--skip-test-tvos-host',
        action=arguments.action.disable,
        dest='test_tvos_host',
        help='skip testing tvOS device targets on the host machine (the TV '
             'itself)')
    skip_test_group.add_argument(
        '--skip-test-watchos',
        action=arguments.action.disable,
        dest='test_watchos',
        help='skip testing all tvOS targets. Equivalent to specifying both '
             '--skip-test-watchos-simulator and --skip-test-watchos-host')
    skip_test_group.add_argument(
        '--skip-test-watchos-simulator',
        action=arguments.action.disable,
        dest='test_watchos_simulator',
        help='skip testing watchOS simulator targets')
    skip_test_group.add_argument(
        '--skip-test-watchos-host',
        action=arguments.action.disable,
        dest='test_watchos_host',
        help='skip testing watchOS device targets on the host machine (the '
             'watch itself)')
    skip_test_group.add_argument(
        '--skip-test-android-host',
        action=arguments.action.disable,
        dest='test_android_host',
        help='skip testing Android device targets on the host machine (the '
             'phone itself)')

    # -------------------------------------------------------------------------
    in_group('Build settings specific for LLVM')

    option('--llvm-targets-to-build', store,
           default='X86;ARM;AArch64;PowerPC;SystemZ;Mips',
           help='LLVM target generators to build')

    # -------------------------------------------------------------------------
    in_group('Build settings for Android')

    option('--android-ndk', store_path,
           help='An absolute path to the NDK that will be used as a libc '
                'implementation for Android builds')

    option('--android-api-level', store,
           default='21',
           help='The Android API level to target when building for Android. '
                'Currently only 21 or above is supported')

    option('--android-ndk-gcc-version', store,
           choices=['4.8', '4.9'],
           default='4.9',
           help='The GCC version to use when building for Android. Currently '
                'only 4.9 is supported. %(default)s is also the default '
                'value. This option may be used when experimenting with '
                'versions of the Android NDK not officially supported by '
                'Swift')

    option('--android-icu-uc', store_path,
           help='Path to a directory containing libicuuc.so')
    option('--android-icu-uc-include', store_path,
           help='Path to a directory containing headers for libicuuc')
    option('--android-icu-i18n', store_path,
           help='Path to a directory containing libicui18n.so')
    option('--android-icu-i18n-include', store_path,
           help='Path to a directory containing headers libicui18n')
    option('--android-deploy-device-path', store_path,
           default=android.adb.commands.DEVICE_TEMP_DIR,
           help='Path on an Android device to which built Swift stdlib '
                'products will be deployed. If running host tests, specify '
                'the "{}" directory.'.format(
                    android.adb.commands.DEVICE_TEMP_DIR))

    # -------------------------------------------------------------------------
    return builder.build()


# ----------------------------------------------------------------------------

USAGE = """
  %(prog)s [-h | --help] [OPTION ...]
  %(prog)s --preset=NAME [SUBSTITUTION ...]
"""


DESCRIPTION = """
Use this tool to build, test, and prepare binary distribution archives of Swift
and related tools.

Builds Swift (and, optionally, LLDB), incrementally, optionally
testing it thereafter.  Different build configurations are maintained in
parallel.
"""


EPILOG = """
Using option presets:

  --preset-file=PATH    load presets from the specified file

  --preset=NAME         use the specified option preset

  The preset mode is mutually exclusive with other options.  It is not
  possible to add ad-hoc customizations to a preset.  This is a deliberate
  design decision.  (Rationale: a preset is a certain important set of
  options that we want to keep in a centralized location.  If you need to
  customize it, you should create another preset in a centralized location,
  rather than scattering the knowledge about the build across the system.)

  Presets support substitutions for controlled customizations.  Substitutions
  are defined in the preset file.  Values for substitutions are supplied
  using the name=value syntax on the command line.


Any arguments not listed are forwarded directly to Swift's
'build-script-impl'.  See that script's help for details.

Environment variables
---------------------

This script respects a few environment variables, should you
choose to set them:

SWIFT_SOURCE_ROOT: a directory containing the source for LLVM, Clang, Swift.
                   If this script is located in a Swift
                   source directory, the location of SWIFT_SOURCE_ROOT will be
                   inferred if the variable is not set.

'build-script' expects the sources to be laid out in the following way:

   $SWIFT_SOURCE_ROOT/llvm
                     /clang
                     /swift
                     /lldb                       (optional)
                     /llbuild                    (optional)
                     /swiftpm                    (optional, requires llbuild)
                     /compiler-rt                (optional)
                     /swift-corelibs-xctest      (optional)
                     /swift-corelibs-foundation  (optional)
                     /swift-corelibs-libdispatch (optional)
                     /icu                        (optional)

SWIFT_BUILD_ROOT: a directory in which to create out-of-tree builds.
                  Defaults to "$SWIFT_SOURCE_ROOT/build/".

Preparing to run this script
----------------------------

  See README.md for instructions on cloning Swift subprojects.

If you intend to use the -l, -L, --lldb, or --debug-lldb options.

That's it; you're ready to go!

Examples
--------

Given the above layout of sources, the simplest invocation of 'build-script' is
just:

  [~/src/s]$ ./swift/utils/build-script

This builds LLVM, Clang, Swift and Swift standard library in debug mode.

All builds are incremental.  To incrementally build changed files, repeat the
same 'build-script' command.

Typical uses of 'build-script'
------------------------------

To build everything with optimization without debug information:

  [~/src/s]$ ./swift/utils/build-script -R

To run tests, add '-t':

  [~/src/s]$ ./swift/utils/build-script -R -t

To run normal tests and validation tests, add '-T':

  [~/src/s]$ ./swift/utils/build-script -R -T

To build LLVM+Clang with optimization without debug information, and a
debuggable Swift compiler:

  [~/src/s]$ ./swift/utils/build-script -R --debug-swift

To build a debuggable Swift standard library:

  [~/src/s]$ ./swift/utils/build-script -R --debug-swift-stdlib

iOS build targets are always configured and present, but are not built by
default.  To build the standard library for OS X, iOS simulator and iOS device:

  [~/src/s]$ ./swift/utils/build-script -R -i

To run OS X and iOS tests that don't require a device:

  [~/src/s]$ ./swift/utils/build-script -R -i -t

To use 'make' instead of 'ninja', use '-m':

  [~/src/s]$ ./swift/utils/build-script -m -R

To create Xcode projects that can build Swift, use '-x':

  [~/src/s]$ ./swift/utils/build-script -x -R

Preset mode in build-script
---------------------------

All buildbots and automated environments use 'build-script' in *preset mode*.
In preset mode, the command line only specifies the preset name and allows
limited customization (extra output paths).  The actual options come from
the selected preset in 'utils/build-presets.ini'.  For example, to build like
the incremental buildbot, run:

  [~/src/s]$ ./swift/utils/build-script --preset=buildbot_incremental

To build with AddressSanitizer:

  [~/src/s]$ ./swift/utils/build-script --preset=asan

To build a root for Xcode XYZ, '/tmp/xcode-xyz-root.tar.gz':

  [~/src/s]$ ./swift/utils/build-script --preset=buildbot_BNI_internal_XYZ \\
      install_destdir="/tmp/install"
      install_symroot="/tmp/symroot"
      installable_package="/tmp/xcode-xyz-root.tar.gz"

If you have your own favorite set of options, you can create your own, local,
preset.  For example, let's create a preset called 'ds' (which stands for
Debug Swift):

  $ cat > ~/.swift-build-presets
  [preset: ds]
  release
  debug-swift
  debug-swift-stdlib
  test
  build-subdir=ds

To use it, specify the '--preset=' argument:

  [~/src/s]$ ./swift/utils/build-script --preset=ds
  ./swift/utils/build-script: using preset 'ds', which expands to
  ./swift/utils/build-script --release --debug-swift --debug-swift-stdlib \
     --test
  --build-subdir=ds --
  ...

Existing presets can be found in `utils/build-presets.ini`

Philosophy
----------

While you can invoke CMake directly to build Swift, this tool will save you
time by taking away the mechanical parts of the process, providing you controls
for the important options.

For all automated build environments, this tool is regarded as *the* *only* way
to build Swift.  This is not a technical limitation of the Swift build system.
It is a policy decision aimed at making the builds uniform across all
environments and easily reproducible by engineers who are not familiar with the
details of the setups of other systems or automated environments.
"""
