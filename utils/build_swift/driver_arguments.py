# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import argparse
import multiprocessing

import android.adb.commands

# TODO: Replace with implementation in build_swift package
from swift_build_support.swift_build_support import host
from swift_build_support.swift_build_support import arguments
from swift_build_support.swift_build_support.targets import \
    StdlibDeploymentTarget

from . import defaults
from .argparse_builder import ArgumentParserBuilder


__all__ = [
    'create_argument_parser',
]


def create_argument_parser():
    """Return configured argument parser for main driver."""

    # NOTE: USAGE, DESCRIPTION and EPILOG are defined at the bottom of the file
    builder = ArgumentParserBuilder(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        usage=USAGE,
        description=DESCRIPTION,
        epilog=EPILOG)

    # Prepare DSL functions
    in_group = builder.in_group
    option = builder.add_option
    set_defaults = builder.set_defaults
    mutually_exclusive_group = builder.mutually_exclusive_group

    set_ = builder.set_action
    set_int = builder.set_int_action
    set_path = builder.set_path_action
    set_true = builder.set_true_action
    set_false = builder.set_false_action
    disable = builder.disable_action
    enable = builder.enable_action
    append = builder.append_action
    unsupported = builder.unuspported_action

    # -------------------------------------------------------------------------
    # Top-level options

    option(['-n', '--dry-run'], set_true,
           help='print the commands that would be executed, but do not '
                'execute them')

    option(['-c', '--clean'], set_true,
           help='clean the build directory')

    # TODO: rename to '-v' and '--verbose'
    option('--verbose-build', enable,
           help='print the commands executed during the build of each project')

    option('--show-sdks', enable,
           help='print the installed Xcode and SDK versions')

    option('--no-legacy-impl', set_false('legacy_impl'),
           help='avoid legacy build-script-impl implementation')

    option('--export-compile-commands', enable,
           help='generate compilation databases in addition to building')

    option('--symbols-package', set_path,
           help='path to output the generated archive of the symbols '
                'directory')

    # -------------------------------------------------------------------------
    in_group('Workspace Settings')

    option('--build-subdir', set_path,
           help='name of the directory under $SWIFT_BUILD_ROOT where the '
                'build products will be placed')

    option('--darwin-xcrun-toolchain', set_,
           default='default',
           metavar='NAME',
           help='name of the toolchain to use on Darwin')

    option('--build-args', append(separator=' '),
           help='arguments to the build tool')

    option(['-j', '--jobs'], set_int('build_jobs'),
           default=multiprocessing.cpu_count(),
           help='number of parallel build jobs to use')

    option('--cmake', set_path,
           help='path to a CMake executable that will be used to '
                'build Swift')

    option('--host-cc', set_path,
           help='path to CC, the "clang" compiler for the host platform. '
                '(default is auto detected)')

    option('--host-cxx', set_path,
           help='path to CXX, the "clang++" compiler for the host '
                'platform. (default is auto detected)')

    option('--host-lipo', set_path,
           help='path to lipo (default is auto detected)')

    option('--host-libtool', set_path,
           help='path to lipo (default is auto detected)')

    option("--distcc", enable,
           help='use distcc in pump mode')

    # -----------------------------------------------------------------------
    in_group('Compiler options')

    option('--compiler-vendor', set_,
           choices=['none', 'apple'],
           default=defaults.COMPILER_VENDOR,
           metavar='VENDOR',
           help='compiler vendor name')

    option('--clang-compiler-version', set_,
           type=arguments.type.clang_compiler_version,
           metavar='VERSION',
           help='Clang compiler version')

    option('--clang-user-visible-version', set_,
           type=arguments.type.clang_compiler_version,
           default=defaults.CLANG_USER_VISIBLE_VERSION,
           metavar='VERSION',
           help='user-visible version of the embedded Clang and LLVM '
                'compilers')

    option('--swift-compiler-version', set_,
           type=arguments.type.swift_compiler_version,
           metavar='VERSION',
           help='Swift compiler version')

    option('--swift-user-visible-version', set_,
           type=arguments.type.swift_compiler_version,
           default=defaults.SWIFT_USER_VISIBLE_VERSION,
           metavar='VERSION',
           help='user-visible version of the embedded Swift compiler')

    # -------------------------------------------------------------------------
    in_group('CMake')

    # FIXME: This should be one option using choices
    with mutually_exclusive_group():
        set_defaults(cmake_generator=defaults.CMAKE_GENERATOR)

        # option('--cmake-generator', set_,
        #        help='manually select the CMake generator')

        # option('--ninja',
        #        set_('cmake_generator', const='Ninja'),
        #        help='use Cmake\'s Ninja generator (default)')

        option(['-x', '--xcode'],
               set_('cmake_generator', const='Xcode'),
               help='use CMake\'s Xcode generator (default is Ninja)')

        option(['-m', '--make'],
               set_('cmake_generator', const='Unix Makefiles'),
               help='use CMake\'s Makefile generator (default is Ninja)')

        option(['-e', '--eclipse'],
               set_('cmake_generator', const='Eclipse CDT4 - Ninja'),
               help='use CMake\'s Makefile generator (default is Ninja)')

    # TODO: Remove the 'extra' variant in favor of simpler '--cmake-options'
    # TODO: Add '-D' option to directly pass CMake arguments
    option('--extra-cmake-options',
           append('extra_cmake_options', separator=','),
           help='pass through extra options to CMake in the form of comma '
                'separated options "-DCMAKE_VAR1=YES,-DCMAKE_VAR2=/tmp". '
                'Can be called multiple times to add multiple such options.')

    # -------------------------------------------------------------------------
    in_group('Build Variants')

    with mutually_exclusive_group():
        all_build_variants = [
            # TODO: Remove once build-script no longer depends on destination
            'build_variant',

            'cmark_build_variant',
            'foundation_build_variant',
            'llbuild_build_variant',
            'lldb_build_variant',
            'libdispatch_build_variant',
            'libicu_build_variant',
            'llvm_build_variant',
            'swift_build_variant',
            'swift_stdlib_build_variant',
        ]

        set_defaults(all_build_variants, defaults.BUILD_VARIANT)

        option(['-d', '--debug'],
               set_(all_build_variants, const='Debug'),
               help='build the Debug variant of everything (LLVM, Clang, '
                    'Swift host tools, target Swift standard libraries, LLDB '
                    '(if enabled) (default)')

        option(['-r', '--release-debuginfo'],
               set_(all_build_variants, const='RelWithDebInfo'),
               help='build the RelWithDebInfo variant of everything (default '
                    'is Debug)')

        option(['-R', '--release'],
               set_(all_build_variants, const='Release'),
               help='build the Release variant of everything (default '
                    'is Debug)')

    # Valid CMake build variants
    # build_variant_choices = [
    #     'Debug',
    #     'RelWithDebInfo',
    #     'Release',
    #     'MinSizeRel',
    # ]

    option('--debug-cmark',
           set_('cmark_build_variant', const='Debug'),
           help='build the Debug variant of CommonMark')

    option('--debug-foundation',
           set_('foundation_build_variant', const='Debug'),
           help='build the Debug variant of Foundation')

    option('--debug-lldb',
           set_('lldb_build_variant', const='Debug'),
           set_true('build_lldb'),
           help='build the Debug variant of LLDB')

    option('--debug-libdispatch',
           set_('libdispatch_build_variant', const='Debug'),
           help='build the Debug variant of libdispatch')

    option('--debug-libicu',
           set_('libicu_build_variant', const='Debug'),
           help='build the Debug variant of libdicu')

    option('--debug-llvm',
           set_('llvm_build_variant', const='Debug'),
           help='build the Debug variant of LLVM')

    option('--debug-swift',
           set_('swift_build_variant', const='Debug'),
           help='build the Debug variant of Swift')

    option('--debug-swift-stdlib',
           set_('swift_stdlib_build_variant', const='Debug'),
           help='build the Debug variant of the Swift standard library')

    # -------------------------------------------------------------------------
    in_group('Host and cross-compilation build options')

    option('--host-target', set_,
           default=StdlibDeploymentTarget.host_target().name,
           metavar='TARGET',
           help='the host target. LLVM, Clang, and Swift will be built for '
                'this target. The built LLVM and Clang will be used to '
                'compile Swift for the cross-compilation targets.')

    option('--cross-compile-hosts', append(separator=' '),
           metavar='TARGET',
           help='space-separated list of targets to cross-compile the host '
                'Swift tools for. Can be used multiple times.')

    option('--stdlib-deployment-targets', append(separator=' '),
           metavar='TARGET',
           default=None,
           help='space-separated list of targets to compile or cross-compile '
                'the Swift standard library for. %(default)s by default')

    option('--build-stdlib-deployment-targets', append(separator=' '),
           default=['all'],
           metavar='TARGET',
           help='space-separated list that filters which of the configured '
                'targets to build the Swift standard library for, or "all"')

    option('--skip-build-osx', disable('build_osx'),
           help='skip building Swift stdlibs for OS X')

    option('--skip-build-linux', disable('build_linux'),
           help='skip building Swift stdlibs for Linux')

    option('--skip-build-freebsd', disable('build_freebsd'),
           help='skip building Swift stdlibs for FreeBSD')

    option('--skip-build-cygwin', disable('build_cygwin'),
           help='skip building Swift stdlibs for Cygwin')

    all_ios = [
        # FIXME: Remove once build-script no longer depends on destination
        'ios',
        'test_ios',

        'build_ios',
        'build_ios_device',
        'build_ios_simulator',
        'test_ios_host',
        'test_ios_simulator',
    ]

    all_tvos = [
        # FIXME: Remove once build-script no longer depends on destination
        'tvos',
        'test_tvos',

        'build_tvos',
        'build_tvos_device',
        'build_tvos_simulator',
        'test_tvos_host',
        'test_tvos_simulator',
    ]

    all_watchos = [
        # FIXME: Remove once build-script no longer depends on destination
        'watchos',
        'test_watchos',

        'build_watchos',
        'build_watchos_device',
        'build_watchos_simulator',
        'test_watchos_host',
        'test_watchos_simulator',
    ]

    set_defaults(all_ios, False)
    set_defaults(all_tvos, False)
    set_defaults(all_watchos, False)

    set_defaults(build_ios=True)
    set_defaults(build_tvos=True)
    set_defaults(build_watchos=True)

    option(['-i', '--ios'], enable(all_ios),
           help='also build for iOS, but disallow tests that require an '
                'iOS device')
    option('--skip-ios', set_false(all_ios),
           help='skip everything iOS-related')
    option('--skip-build-ios',
           disable(all_ios),
           help='skip building Swift stdlibs for iOS')
    option('--skip-build-ios-device',
           disable('build_ios_device'),
           help='skip building Swift stdlibs for iOS devices (i.e. '
                'build simulators only)')
    option('--skip-build-ios-simulator',
           disable('build_ios_simulator'),
           help='skip building Swift stdlibs for iOS simulators (i.e. '
                'build devices only)')

    option('--tvos', enable(all_tvos),
           help='also build for tvOS, but disallow tests that require an '
                'tvOS device')
    option('--skip-tvos', set_false(all_tvos),
           help='skip everything tvOS-related')
    option('--skip-build-tvos',
           disable(all_tvos),
           help='skip building Swift stdlibs for tvOS')
    option('--skip-build-tvos-device',
           disable('build_tvos_device'),
           help='skip building Swift stdlibs for tvOS devices (i.e. '
                'build simulators only)')
    option('--skip-build-tvos-simulator',
           disable('build_tvos_simulator'),
           help='skip building Swift stdlibs for tvOS simulators (i.e. '
                'build devices only)')

    option('--watchos', enable(all_watchos),
           help='also build for watchOS, but disallow tests that require an '
                'watchOS device')
    option('--skip-watchos', set_false(all_watchos),
           help='skip everything watchOS-related')
    option('--skip-build-watchos',
           disable(all_watchos),
           help='skip building Swift stdlibs for watchOS')
    option('--skip-build-watchos-device',
           disable('build_watchos_device'),
           help='skip building Swift stdlibs for watchOS devices (i.e. '
                'build simulators only)')
    option('--skip-build-watchos-simulator',
           disable('build_watchos_simulator'),
           help='skip building Swift stdlibs for watchOS simulators (i.e. '
                'build devices only)')

    option('--darwin-deployment-version-osx', set_,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_OSX,
           metavar='VERSION',
           help='minimum deployment target for OS X')

    option('--darwin-deployment-version-ios', set_,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_IOS,
           metavar='VERSION',
           help='minimum deployment target for iOS')

    option('--darwin-deployment-version-tvos', set_,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_TVOS,
           metavar='VERSION',
           help='minimum deployment target for tvOS')

    option('--darwin-deployment-version-watchos', set_,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_WATCHOS,
           metavar='VERSION',
           help='minimum deployment target for watchOS')

    # -------------------------------------------------------------------------
    in_group('Project build options')

    # Override to force bootstrap Ninja
    option('--build-ninja', enable,
           help='build Ninja')

    all_required_project_builds = [
        'build_cmark',
        'build_llvm',
        'build_swift',
    ]

    all_optional_project_builds = [
        'build_foundation',
        'build_llbuild',
        'build_lldb',
        'build_libdispatch',
        'build_libicu',
        'build_playgroundlogger',
        'build_playgroundsupport',
        'build_swiftpm',
        'build_xctest',
    ]

    all_build_platforms = [
        'build_osx',
        'build_ios',
        'build_tvos',
        'build_watchos',
        'build_benchmarks',
        'build_linux',
        'build_android',
        'build_freebsd',
        'build_cygwin',
    ]

    set_defaults(all_required_project_builds, True)
    set_defaults(all_optional_project_builds, False)

    all_project_builds = (
        all_required_project_builds +
        all_optional_project_builds)

    # FIXME: Remove once build-script no longer depends on destination
    set_defaults(skip_build=False)

    option(['-S', '--skip-build'],
           # FIXME: Remove once build-script no longer depends on destination
           enable('skip_build'),

           disable(all_project_builds),
           disable(all_build_platforms),
           help='generate the build directory without building')

    option('--foundation',
           enable('build_foundation'),
           help='build foundation')
    option(['-b', '--llbuild'],
           enable('build_llbuild'),
           help='build llbuild')
    option(['-l', '--lldb'],
           enable('build_lldb'),
           help='build LLDB')
    option('--libdispatch',
           enable('build_libdispatch'),
           help='build libdispatch')
    option('--libicu',
           enable('build_libicu'),
           help='build libicu')
    option('--playgroundlogger',
           enable('build_playgroundlogger'),
           help='build PlaygroundLogger')
    option('--playgroundsupport',
           enable('build_playgroundsupport'),
           help='build PlaygroundSupport')
    option(['-p', '--swiftpm'],
           enable('build_swiftpm'),
           help='build swiftpm')

    # XCTest has a dependency on Foundation.
    # On OS X, Foundation is built automatically using xcodebuild.
    # On Linux, we must ensure that it is built manually.
    build_xctest_dst = ['build_xctest']
    if not StdlibDeploymentTarget.host_target().platform.is_darwin:
        build_xctest_dst.append('build_foundation')

    option('--xctest',
           enable(build_xctest_dst),
           help='build XCTest')

    # -----------------------------------------------------------------------
    in_group('LLVM/Clang build options')

    option('--llvm-targets-to-build', set_,
           default='X86;ARM;AArch64;PowerPC;SystemZ;Mips',
           help='LLVM target generators to build')

    option('--clang-profile-instr-use', set_path,
           help='profile file to use for clang PGO')

    # -------------------------------------------------------------------------
    in_group('Swift build options')

    option('--swift-analyze-code-coverage', set_,
           choices=['false', 'not-merged', 'merged'],
           default='false',  # so CMake can see the inert mode as a false value
           help='enable code coverage analysis in Swift')

    option('--build-runtime-with-host-compiler', enable,
           help='use the host compiler, not the self-built one to compile the '
                'Swift runtime')

    option('--extra-swift-args', append,
           metavar='"MODULE_REGEXP;FLAG"',
           help='pass through extra flags to swift in the form of a cmake '
                'list "module_regexp;flag". Can be called multiple times to '
                'add multiple such module_regexp flag pairs. All semicolons '
                'in flags must be escaped with a "\\"'),

    option('--build-swift-stdlib-unittest-extra', enable,  # See tests sec.
           help='build optional StdlibUnittest components')

    option('--build-swift-dynamic-stdlib', enable,
           default=True,
           help='build dynamic versions of the Swift standard library')

    option('--build-swift-static-stdlib', enable,
           help='build static versions of the Swift standard library')

    option('--build-swift-dynamic-sdk-overlay', enable,
           default=True,
           help='build dynamic variants of the Swift SDK overlay')

    option('--build-swift-static-sdk-overlay', enable,
           help='build static variants of the Swift SDK overlay')

    option('--enable-sil-ownership', enable,
           help='enable the SIL ownership model')

    option('--force-optimized-typechecker', enable,
           help='Force the type checker to be built with optimization')

    # -------------------------------------------------------------------------
    in_group('Link-Time Optimization (LTO)')

    option('--lto', set_('lto_type', const='full'),
           choices=['thin', 'full'],
           default=None,
           metavar='LTO_TYPE',
           help='use lto optimization on llvm/swift tools. This does not '
                'imply using lto on the swift standard library or runtime. '
                'Options: thin, full. If no optional arg is provided, full is '
                'chosen by default')

    max_lto_link_job_counts = host.max_lto_link_job_counts()

    option('--llvm-max-parallel-lto-link-jobs', set_,
           default=max_lto_link_job_counts['llvm'],
           metavar='COUNT',
           help='the maximum number of parallel link jobs to use when '
                'compiling llvm')

    option('--swift-tools-max-parallel-lto-link-jobs', set_,
           default=max_lto_link_job_counts['swift'],
           metavar='COUNT',
           help='the maximum number of parallel link jobs to use when '
                'compiling swift tools.')

    # -------------------------------------------------------------------------
    in_group('Assertions')

    with mutually_exclusive_group():
        all_assertions = [
            # FIXME: Remove once build-script no longer depends on destination
            'assertions',

            'llbuild_assertions',
            'llvm_assertions',
            'swift_assertions',
            'swift_stdlib_assertions',

            # FIXME: these values are not used in build-script-impl
            'cmark_assertions',
            'lldb_assertions',
        ]

        set_defaults(all_assertions, True)

        # FIXME: Set to a more suitable default value
        set_defaults(lldb_assertions=None)

        option('--assertions', set_true(all_assertions),
               help='enable assertions in all projects')
        option('--no-assertions', set_false(all_assertions),
               help='disable assertions in all projects')

    # FIXME: cmark_assertions is not used by build-script-impl
    option('--cmark-assertions',
           set_('cmark_assertions', const=True),
           help='enable assertions in CommonMark')

    # FIXME: lldb_assertions is not used by build-script-impl
    option('--lldb-assertions',
           set_('lldb_assertions', const=True),
           set_true('build_lldb'),
           default=None,
           help='enable assertions in LLDB')
    option('--no-lldb-assertions',
           set_false('lldb_assertions'),
           help='disable assertions in LLDB')

    option('--llvm-assertions',
           set_('llvm_assertions', const=True),
           help='enable assertions in LLVM and Clang')
    option('--no-llvm-assertions',
           set_false('llvm_assertions'),
           help='disable assertions in LLVM and Clang')

    option('--swift-assertions',
           set_('swift_assertions', const=True),
           help='enable assertions in Swift')
    option('--no-swift-assertions',
           set_false('swift_assertions'),
           help='disable assertions in Swift')

    option('--swift-stdlib-assertions',
           set_('swift_stdlib_assertions', const=True),
           help='enable assertions in the Swift standard library')
    option('--no-swift-stdlib-assertions',
           set_false('swift_stdlib_assertions'),
           help='disable assertions in the Swift standard library')

    # -------------------------------------------------------------------------
    in_group('Sanitizers')

    option('--enable-asan', enable,
           help='enable Address Sanitizer')

    option('--enable-ubsan', enable,
           help='enable Undefined Behavior Sanitizer')

    option('--enable-tsan', enable,
           help='enable Thread Sanitizer for Swift tools')

    # FIXME: Should this be a boolean value?
    option('--enable-tsan-runtime', set_,
           help='enable Thread Sanitizer for the Swift runtime')

    option('--enable-lsan', enable,
           help='enable Leak Sanitizer for Swift tools')

    # -----------------------------------------------------------------------
    in_group('Tests')

    all_project_tests = [
        'test_cmark',
        'test_foundation',
        'test_libdispatch'
        'test_libicu'
        'test_llbuild',
        'test_lldb',
        'test_playgroundlogger',
        'test_playgroundsupport',
        'test_swift',
        'test_swiftpm',
        'test_xctest',
    ]

    all_os_tests = [
        'test_osx'
        'test_linux',
        'test_freebsd',
        'test_cygwin',
    ]

    all_normal_tests = all_project_tests + all_os_tests

    set_defaults(all_normal_tests, False)
    set_defaults(test_optimized=False)

    # FIXME: Set to a more suitable default value
    set_defaults(validation_test=None)

    # FIXME: Remove once build-script no longer depends on destination
    set_defaults(test=None)
    all_normal_tests += ['test']

    # FIXME: Compine into single option
    option('-t',
           set_('test', const=True),
           set_true(all_normal_tests),
           help='test Swift after building')
    option('--test', enable(all_normal_tests),
           help='test Swift after building')

    # FIXME: Combine into single option
    option('-T',
           set_('validation_test', const=True),
           set_true(all_normal_tests),
           help='run the validation test suite (implies --test)')
    option('--validation-test',
           enable('validation_test'),
           enable(all_normal_tests),
           help='run the validation test suite (implies --test)')

    # FIXME: Set to a more suitable default value
    set_defaults(test_optimized=None)
    # FIXME: Combine into single option
    option('-o',
           set_('test_optimized', const=True),
           set_true(all_normal_tests),
           help='run the test suite in optimized mode too (implies '
                '--test')
    option('--test-optimized',
           enable('test_optimized'),
           enable(all_normal_tests),
           help='run the test suite in optimized mode too (implies '
                '--test)')

    # FIXME: Set to a more suitable default value
    set_defaults(test_optimize_for_size=None)
    # FIXME: Combine into single option
    option('-s',
           set_('test_optimize_for_size', const=True),
           set_true(all_normal_tests),
           help='run the test suite in optimize for size mode too (implies'
                ' --test)')
    option('--test-optimize-for-size',
           enable('test_optimize_for_size'),
           enable(all_normal_tests),
           help='run the test suite in optimize for size mode too (implies'
                ' --test')

    option('--long-test', enable,
           help='run the long test suite')

    # TODO: Rename destination to 'test_host'
    option('--host-test', enable('host_test'),
           help='run executable tests on host devices (such as iOS or tvOS)')

    # TODO: Convert to append action
    option('--lit-args', set_,
           default='-sv',
           metavar='LITARG',
           help='lit args to use when testing')

    option('--coverage-db', set_path,
           help='coverage database to use when prioritizing testing')

    # FIXME: test_paths has extra logic that must live here to enable
    # test or validation test
    option('--test-paths', append,
           help='run tests located in specific directories and/or files'
                '(implies --test and/or --validation-test)')

    # FIXME: Set to more suitable default values
    set_defaults(test_osx=False)
    set_defaults(test_linux=False)

    option('--skip-test-osx', disable('test_osx'),
           help='skip testing Swift stdlibs for OS X')
    option('--skip-test-linux', disable('test_linux'),
           help='skip testing Swift stdlibs for Linux')
    option('--skip-test-freebsd', disable('test_freebsd'),
           help='skip testing Swift stdlibs for FreeBSD')
    option('--skip-test-cygwin', disable('test_cygwin'),
           help='skip testing Swift stdlibs for Cygwin')

    option('--skip-test-ios',
           # FIXME: Remove once build-script no longer depends on destination
           disable('test_ios'),
           disable(['test_ios_simulator', 'test_ios_host']),
           help='skip testing all iOS targets. Equivalent to specifying both '
                '--skip-test-ios-simulator and --skip-test-ios-host')
    option('--skip-test-ios-simulator',
           disable('test_ios_simulator'),
           help='skip testing iOS simulator targets')
    option('--skip-test-ios-32bit-simulator',
           disable('test_ios_32bit_simulator'),
           help='skip testing iOS 32bit simulator targets')
    option('--skip-test-ios-host',
           disable('test_ios_host'),
           help='skip testing iOS device targets on the host machine (the '
                'phone itself)')

    option('--skip-test-tvos',
           # FIXME: Remove once build-script no longer depends on destination
           disable('test_tvos'),
           disable(['test_tvos_simulator', 'test_tvos_host']),
           help='skip testing all tvOS targets. Equivalent to specifying both '
                '--skip-test-tvos-simulator and --skip-test-tvos-host')
    option('--skip-test-tvos-simulator',
           disable('test_tvos_simulator'),
           help='skip testing tvOS simulator targets')
    option('--skip-test-tvos-host',
           disable('test_tvos_host'),
           help='skip testing tvOS device targets on the host machine (the '
                'phone itself)')

    option('--skip-test-watchos',
           # FIXME: Remove once build-script no longer depends on destination
           disable('test_watchos'),
           disable(['test_watchos_simulator', 'test_watchos_host']),
           help='skip testing all watchOS targets. Equivalent to specifying '
                'both --skip-test-watchos-simulator and '
                '--skip-test-watchos-host')
    option('--skip-test-watchos-simulator',
           disable('test_watchos_simulator'),
           help='skip testing watchOS simulator targets')
    option('--skip-test-watchos-host',
           disable('test_watchos_host'),
           help='skip testing watchOS device targets on the host machine (the '
                'phone itself)')

    # ------------------------------------------------------------------------
    in_group('Benchmarks')

    option('--skip-build-benchmarks', disable('build_benchmarks'),
           help='skip building the Swift benchmark suite')

    option(['-B', '--benchmark'],
           set_true('benchmark'),
           help='run the Swift benchmark suite after building')

    option('--build-external-benchmarks', enable,
           help='skip building Swift Benchmark Suite')

    option('--benchmark-num-o-iterations', set_int,
           default=3,
           help='if the Swift Benchmark Suite is run after building, run N '
                'iterations with -O')

    option('--benchmark-num-onone-iterations', set_int,
           default=3,
           help='if the Swift Benchmark Suite is run after building, run N '
                'iterations with -Onone')

    # ------------------------------------------------------------------------
    in_group('Installation')

    if StdlibDeploymentTarget.host_target().platform.is_darwin:
        set_defaults(install_prefix=defaults.DARWIN_INSTALL_PREFIX)
    else:
        set_defaults(install_prefix=defaults.UNIX_INSTALL_PREFIX)

    option('--install-prefix', set_path,
           help='installation prefix. This is where built Swift products '
                '(like bin, lib, and include) will be installed.')

    option('--install-symroot', set_path,
           help='path to install debug symbols into')

    # -----------------------------------------------------------------------
    in_group('Android build options')

    with mutually_exclusive_group():
        set_defaults(build_android=False)

        # TODO: add --build-android option
        # TODO: rename destination to build_android
        option('--android', enable,
               help='also build for Android')
        option('--skip-build-android', disable('build_android'),
               help='skip building Swift stdlibs for Android')

    option('--skip-test-android-host',
           disable('test_android_host'),
           default=False,
           help='skip testing Android device targets on the host machine (the '
                'phone iteself')

    option('--android-ndk', set_path,
           help='an absolute path to the NDK that will be used as a libc '
                'implementation for Android builds')

    option('--android-api-level', set_,
           default=defaults.ANDROID_API_LEVEL,
           help='the Android API level to target when building for Android. '
                'Currently only 21 or above is supported.')

    option('--android-ndk-gcc-version', set_,
           choices=['4.8', '4.9'],
           default=defaults.ANDROID_NDK_GCC_VERSION,
           help='the GCC version to use when building for Android. Currently '
                'only 4.9 is supported. %(default)s is also the default '
                'value. This option may be used when experimenting with '
                'versions of the Android NDK not officially supported by '
                'Swift')

    option('--android-icu-uc', set_path,
           help='path to a directory containing libicuuc.so')

    option('--android-icu-uc-include', set_path,
           help='path to a directory containing headers for libicuuc')

    option('--android-icu-i18n', set_path,
           help='path to a directory containing libicui18n.so')

    option('--android-icu-i18n-include', set_path,
           help='path to a directory containing headers libicui18n')

    option('--android-deploy-device-path', set_path,
           default=android.adb.commands.DEVICE_TEMP_DIR,
           help='path on an Android device to which built Swift stdlib '
                'products will be deployed. If running host tests, specify '
                'the "%(default)s" directory.')

    # ------------------------------------------------------------------------
    in_group('Unsupported options')

    option('--build-jobs', unsupported)
    option('--common-cmake-options', unsupported)
    option('--only-execute', unsupported)
    option('--skip-test-optimize-for-size', unsupported)
    option('--skip-test-optimized', unsupported)

    # FIXME: Remove once build-script no longer depends on destinations
    set_defaults(ios_all=False)
    set_defaults(tvos_all=False)
    set_defaults(watchos_all=False)

    option(['-I', '--ios-all'],
           unsupported(message='--ios-all is unavailable in open-source '
                               'Swift. Use --ios to skip iOS device tests.'))

    option('--tvos-all',
           unsupported(message='--tvos-all is unavailable in open-source '
                               'Swift. Use --tvos to skip tvOS device tests.'))

    option('--watchos-all',
           unsupported(message='--watchos-all is unavailable in open-source '
                               'Swift. Use --watchos to skip watchOS '
                               'device tests.'))

    # ------------------------------------------------------------------------
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
