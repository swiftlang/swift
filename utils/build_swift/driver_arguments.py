# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


import multiprocessing

import android.adb.commands

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
       args.lldb_assertions is not None or \
       args.lldb_build_with_xcode is not None:
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

    if args.lldb_build_with_xcode is None:
        args.lldb_build_with_xcode = '1'

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

    if args.llbuild_assertions is None:
        args.llbuild_assertions = args.assertions

    if args.lldb_assertions is None:
        args.lldb_assertions = args.assertions

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

    # --test-paths implies --test and/or --validation-test
    # depending on what directories/files have been specified.
    if args.test_paths:
        for path in args.test_paths:
            if path.startswith('test'):
                args.test = True
            elif path.startswith('validation-test'):
                args.test = True
                args.validation_test = True

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
    append = builder.actions.append
    store = builder.actions.store
    store_true = builder.actions.store_true
    store_false = builder.actions.store_false
    store_int = builder.actions.store_int
    store_path = builder.actions.store_path
    toggle_true = builder.actions.toggle_true
    toggle_false = builder.actions.toggle_false
    unsupported = builder.actions.unsupported

    # -------------------------------------------------------------------------
    # Top-level options

    option(['-n', '--dry-run'], store_true,
           help='print the commands that would be executed, but do not '
                'execute them')
    option('--no-legacy-impl', store_false('legacy_impl'),
           help='avoid legacy implementation')

    option('--build-runtime-with-host-compiler', toggle_true,
           help='Use the host compiler, not the self-built one to compile the '
                'Swift runtime')

    option(['-i', '--ios'], store_true,
           help='also build for iOS, but disallow tests that require an iOS '
                'device')
    option(['-I', '--ios-all'], store_true('ios_all'),
           help='also build for iOS, and allow all iOS tests')
    option('--skip-ios', store_false('ios'),
           help='set to skip everything iOS-related')

    option('--tvos', toggle_true,
           help='also build for tvOS, but disallow tests that require a tvos '
                'device')
    option('--tvos-all', toggle_true('tvos_all'),
           help='also build for tvOS, and allow all tvOS tests')
    option('--skip-tvos', store_false('tvos'),
           help='set to skip everything tvOS-related')

    option('--watchos', toggle_true,
           help='also build for watchOS, but disallow tests that require an '
                'watchOS device')
    option('--watchos-all', toggle_true('watchos_all'),
           help='also build for Apple watchOS, and allow all Apple watchOS '
                'tests')
    option('--skip-watchos', store_false('watchos'),
           help='set to skip everything watchOS-related')

    option('--android', toggle_true,
           help='also build for Android')

    option('--swift-analyze-code-coverage', store,
           choices=['false', 'not-merged', 'merged'],
           # so CMake can see the inert mode as a false value
           default=defaults.SWIFT_ANALYZE_CODE_COVERAGE,
           help='enable code coverage analysis in Swift (false, not-merged, '
                'merged).')

    option('--build-subdir', store,
           metavar='PATH',
           help='name of the directory under $SWIFT_BUILD_ROOT where the '
                'build products will be placed')
    option('--install-prefix', store_path,
           default=targets.install_prefix(),
           help='The installation prefix. This is where built Swift products '
                '(like bin, lib, and include) will be installed.')
    option('--install-symroot', store_path,
           help='the path to install debug symbols into')

    option(['-j', '--jobs'], store_int('build_jobs'),
           default=multiprocessing.cpu_count(),
           help='the number of parallel build jobs to use')

    option('--darwin-xcrun-toolchain', store,
           help='the name of the toolchain to use on Darwin')
    option('--cmake', store_path(executable=True),
           help='the path to a CMake executable that will be used to build '
                'Swift')
    option('--show-sdks', toggle_true,
           help='print installed Xcode and SDK versions')

    option('--extra-swift-args', append,
           help='Pass through extra flags to swift in the form of a CMake '
                'list "module_regexp;flag". Can be called multiple times to '
                'add multiple such module_regexp flag pairs. All semicolons '
                'in flags must be escaped with a "\\"')

    option('--host-cc', store_path(executable=True),
           help='the absolute path to CC, the "clang" compiler for the host '
                'platform. Default is auto detected.')
    option('--host-cxx', store_path(executable=True),
           help='the absolute path to CXX, the "clang++" compiler for the '
                'host platform. Default is auto detected.')
    option('--host-lipo', store_path(executable=True),
           help='the absolute path to lipo. Default is auto detected.')
    option('--host-libtool', store_path(executable=True),
           help='the absolute path to libtool. Default is auto detected.')
    option('--distcc', toggle_true,
           help='use distcc in pump mode')
    option('--enable-asan', toggle_true,
           help='enable Address Sanitizer')
    option('--enable-ubsan', toggle_true,
           help='enable Undefined Behavior Sanitizer')
    option('--enable-tsan', toggle_true,
           help='enable Thread Sanitizer for swift tools')
    option('--enable-tsan-runtime', toggle_true,
           help='enable Thread Sanitizer on the swift runtime')
    option('--enable-lsan', toggle_true,
           help='enable Leak Sanitizer for swift tools')
    option('--enable-sanitize-coverage', toggle_true,
           help='enable sanitizer coverage for swift tools. Necessary for '
                'fuzzing swiftc')

    option('--compiler-vendor', store,
           choices=['none', 'apple'],
           default=defaults.COMPILER_VENDOR,
           help='Compiler vendor name')
    option('--clang-compiler-version', store,
           type=argparse.ClangVersionType(),
           metavar='MAJOR.MINOR.PATCH',
           help='string that indicates a compiler version for Clang')
    option('--clang-user-visible-version', store,
           type=argparse.ClangVersionType(),
           default=defaults.CLANG_USER_VISIBLE_VERSION,
           metavar='MAJOR.MINOR.PATCH',
           help='User-visible version of the embedded Clang and LLVM '
                'compilers')
    option('--swift-compiler-version', store,
           type=argparse.SwiftVersionType(),
           metavar='MAJOR.MINOR',
           help='string that indicates a compiler version for Swift')
    option('--swift-user-visible-version', store,
           type=argparse.SwiftVersionType(),
           default=defaults.SWIFT_USER_VISIBLE_VERSION,
           metavar='MAJOR.MINOR',
           help='User-visible version of the embedded Swift compiler')

    option('--darwin-deployment-version-osx', store,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_OSX,
           metavar='MAJOR.MINOR',
           help='minimum deployment target version for OS X')
    option('--darwin-deployment-version-ios', store,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_IOS,
           metavar='MAJOR.MINOR',
           help='minimum deployment target version for iOS')
    option('--darwin-deployment-version-tvos', store,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_TVOS,
           metavar='MAJOR.MINOR',
           help='minimum deployment target version for tvOS')
    option('--darwin-deployment-version-watchos', store,
           default=defaults.DARWIN_DEPLOYMENT_VERSION_WATCHOS,
           metavar='MAJOR.MINOR',
           help='minimum deployment target version for watchOS')

    option('--extra-cmake-options', append,
           type=argparse.ShellSplitType(),
           help='Pass through extra options to CMake in the form of comma '
                'separated options "-DCMAKE_VAR1=YES,-DCMAKE_VAR2=/tmp". Can '
                'be called multiple times to add multiple such options.')

    option('--build-args', store,
           type=argparse.ShellSplitType(),
           default=[],
           help='arguments to the build tool. This would be prepended to the '
                'default argument that is "-j8" when CMake generator is '
                '"Ninja".')

    option('--verbose-build', toggle_true,
           help='print the commands executed during the build')

    option('--lto', store('lto_type'),
           choices=['thin', 'full'],
           const='full',
           default=None,
           metavar='LTO_TYPE',
           help='use lto optimization on llvm/swift tools. This does not '
                'imply using lto on the swift standard library or runtime. '
                'Options: thin, full. If no optional arg is provided, full is '
                'chosen by default')

    option('--clang-profile-instr-use', store_path,
           help='profile file to use for clang PGO')

    default_max_lto_link_job_counts = host.max_lto_link_job_counts()
    option('--llvm-max-parallel-lto-link-jobs', store_int,
           default=default_max_lto_link_job_counts['llvm'],
           metavar='COUNT',
           help='the maximum number of parallel link jobs to use when '
                'compiling llvm')

    option('--swift-tools-max-parallel-lto-link-jobs', store_int,
           default=default_max_lto_link_job_counts['swift'],
           metavar='COUNT',
           help='the maximum number of parallel link jobs to use when '
                'compiling swift tools.')

    option('--enable-sil-ownership', store_true,
           help='Enable the SIL ownership model')

    option('--disable-guaranteed-normal-arguments', store_true,
           help='Disable guaranteed normal arguments')

    option('--enable-stdlibcore-exclusivity-checking', store_true,
           help='Enable exclusivity checking in stdlibCore')

    option('--force-optimized-typechecker', store_true,
           help='Force the type checker to be built with '
                'optimization')

    option('--lit-args', store,
           default='-sv',
           metavar='LITARGS',
           help='lit args to use when testing')

    option('--coverage-db', store_path,
           help='coverage database to use when prioritizing testing')

    # -------------------------------------------------------------------------
    in_group('Host and cross-compilation targets')

    option('--host-target', store,
           default=StdlibDeploymentTarget.host_target().name,
           help='The host target. LLVM, Clang, and Swift will be built for '
                'this target. The built LLVM and Clang will be used to '
                'compile Swift for the cross-compilation targets.')

    option('--cross-compile-hosts', append,
           type=argparse.ShellSplitType(),
           default=[],
           help='A space separated list of targets to cross-compile host '
                'Swift tools for. Can be used multiple times.')

    option('--stdlib-deployment-targets', append,
           type=argparse.ShellSplitType(),
           default=None,
           help='list of targets to compile or cross-compile the Swift '
                'standard library for. %(default)s by default.')

    option('--build-stdlib-deployment-targets', store,
           type=argparse.ShellSplitType(),
           default=['all'],
           help='A space-separated list that filters which of the configured '
                'targets to build the Swift standard library for, or "all".')

    # -------------------------------------------------------------------------
    in_group('Options to select projects')

    option(['-l', '--lldb'], store_true('build_lldb'),
           help='build LLDB')

    option(['-b', '--llbuild'], store_true('build_llbuild'),
           help='build llbuild')

    option(['-p', '--swiftpm'], store_true('build_swiftpm'),
           help='build swiftpm')

    option(['--swiftsyntax'], store_true('build_swiftsyntax'),
           help='build swiftSyntax')

    option(['--skstresstester'], store_true('build_skstresstester'),
           help='build the SourceKit stress tester')

    option('--xctest', toggle_true('build_xctest'),
           help='build xctest')

    option('--foundation', toggle_true('build_foundation'),
           help='build foundation')

    option('--libdispatch', toggle_true('build_libdispatch'),
           help='build libdispatch')

    option('--libicu', toggle_true('build_libicu'),
           help='build libicu')

    option('--playgroundsupport', store_true('build_playgroundsupport'),
           help='build PlaygroundSupport')

    option('--build-ninja', toggle_true,
           help='build the Ninja tool')

    # -------------------------------------------------------------------------
    in_group('Extra actions to perform before or in addition to building')

    option(['-c', '--clean'], store_true,
           help='do a clean build')

    option('--export-compile-commands', toggle_true,
           help='generate compilation databases in addition to building')

    option('--symbols-package', store_path,
           help='if provided, an archive of the symbols directory will be '
                'generated at this path')

    # -------------------------------------------------------------------------
    in_group('Build variant')

    with mutually_exclusive_group():

        set_defaults(build_variant='Debug')

        option(['-d', '--debug'], store('build_variant'),
               const='Debug',
               help='build the Debug variant of everything (LLVM, Clang, '
                    'Swift host tools, target Swift standard libraries, LLDB) '
                    '(default is %(default)s)')

        option(['-r', '--release-debuginfo'], store('build_variant'),
               const='RelWithDebInfo',
               help='build the RelWithDebInfo variant of everything (default '
                    'is %(default)s)')

        option(['-R', '--release'], store('build_variant'),
               const='Release',
               help='build the Release variant of everything (default is '
                    '%(default)s)')

    # -------------------------------------------------------------------------
    in_group('Override build variant for a specific project')

    option('--debug-llvm', store('llvm_build_variant'),
           const='Debug',
           help='build the Debug variant of LLVM')

    option('--debug-swift', store('swift_build_variant'),
           const='Debug',
           help='build the Debug variant of Swift host tools')

    option('--debug-swift-stdlib', store('swift_stdlib_build_variant'),
           const='Debug',
           help='build the Debug variant of the Swift standard library and '
                ' SDK overlay')

    option('--debug-lldb', store('lldb_build_variant'),
           const='Debug',
           help='build the Debug variant of LLDB')

    option('--lldb-build-with-xcode', store('lldb_build_with_xcode'),
           const='1',
           help='build LLDB using xcodebuild, if possible')

    option('--lldb-build-with-cmake', store('lldb_build_with_xcode'),
           const='0',
           help='build LLDB using CMake')

    option('--debug-cmark', store('cmark_build_variant'),
           const='Debug',
           help='build the Debug variant of CommonMark')

    option('--debug-foundation', store('foundation_build_variant'),
           const='Debug',
           help='build the Debug variant of Foundation')

    option('--debug-libdispatch', store('libdispatch_build_variant'),
           const='Debug',
           help='build the Debug variant of libdispatch')

    option('--debug-libicu', store('libicu_build_variant'),
           const='Debug',
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

    option('--llbuild-assertions', store,
           const=True,
           help='enable assertions in llbuild')
    option('--no-llbuild-assertions', store('llbuild_assertions'),
           const=False,
           help='disable assertions in llbuild')

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
    in_group('Run tests')

    # NOTE: We can't merge -t and --test, because nargs='?' makes
    #       `-ti` to be treated as `-t=i`.
    # FIXME: Convert to store_true action
    option('-t', store('test', const=True),
           help='test Swift after building')
    option('--test', toggle_true,
           help='test Swift after building')

    option('-T', store('validation_test', const=True),
           help='run the validation test suite (implies --test)')
    option('--validation-test', toggle_true,
           help='run the validation test suite (implies --test)')

    # FIXME: Convert to store_true action
    option('-o', store('test_optimized', const=True),
           help='run the test suite in optimized mode too (implies --test)')
    option('--test-optimized', toggle_true,
           help='run the test suite in optimized mode too (implies --test)')

    # FIXME: Convert to store_true action
    option('-s', store('test_optimize_for_size', const=True),
           help='run the test suite in optimize for size mode too '
                '(implies --test)')
    option('--test-optimize-for-size', toggle_true,
           help='run the test suite in optimize for size mode too '
                '(implies --test)')

    option('--long-test', toggle_true,
           help='run the long test suite')

    option('--stress-test', toggle_true,
           help='run the stress test suite')

    option('--host-test', toggle_true,
           help='run executable tests on host devices (such as iOS or tvOS)')

    option('--test-paths', append,
           type=argparse.ShellSplitType(),
           help='run tests located in specific directories and/or files '
                '(implies --test and/or --validation-test)')

    option(['-B', '--benchmark'], store_true,
           help='run the Swift Benchmark Suite after building')
    option('--benchmark-num-o-iterations', store_int,
           default=3,
           help='if the Swift Benchmark Suite is run after building, run N '
                'iterations with -O')
    option('--benchmark-num-onone-iterations', store_int,
           default=3,
           help='if the Swift Benchmark Suite is run after building, run N '
                'iterations with -Onone')

    option('--skip-test-osx', toggle_false('test_osx'),
           help='skip testing Swift stdlibs for Mac OS X')
    option('--skip-test-linux', toggle_false('test_linux'),
           help='skip testing Swift stdlibs for Linux')
    option('--skip-test-freebsd', toggle_false('test_freebsd'),
           help='skip testing Swift stdlibs for FreeBSD')
    option('--skip-test-cygwin', toggle_false('test_cygwin'),
           help='skip testing Swift stdlibs for Cygwin')

    # -------------------------------------------------------------------------
    in_group('Run build')

    option('--build-swift-dynamic-stdlib', toggle_true,
           default=True,
           help='build dynamic variants of the Swift standard library')

    option('--build-swift-static-stdlib', toggle_true,
           help='build static variants of the Swift standard library')

    option('--build-swift-dynamic-sdk-overlay', toggle_true,
           default=True,
           help='build dynamic variants of the Swift SDK overlay')

    option('--build-swift-static-sdk-overlay', toggle_true,
           help='build static variants of the Swift SDK overlay')

    option('--build-swift-stdlib-unittest-extra', toggle_true,
           help='Build optional StdlibUnittest components')

    option(['-S', '--skip-build'], store_true,
           help='generate build directory only without building')

    option('--skip-build-linux', toggle_false('build_linux'),
           help='skip building Swift stdlibs for Linux')
    option('--skip-build-freebsd', toggle_false('build_freebsd'),
           help='skip building Swift stdlibs for FreeBSD')
    option('--skip-build-cygwin', toggle_false('build_cygwin'),
           help='skip building Swift stdlibs for Cygwin')
    option('--skip-build-osx', toggle_false('build_osx'),
           help='skip building Swift stdlibs for MacOSX')

    option('--skip-build-ios', toggle_false('build_ios'),
           help='skip building Swift stdlibs for iOS')
    option('--skip-build-ios-device', toggle_false('build_ios_device'),
           help='skip building Swift stdlibs for iOS devices '
                '(i.e. build simulators only)')
    option('--skip-build-ios-simulator', toggle_false('build_ios_simulator'),
           help='skip building Swift stdlibs for iOS simulator '
                '(i.e. build devices only)')

    option('--skip-build-tvos', toggle_false('build_tvos'),
           help='skip building Swift stdlibs for tvOS')
    option('--skip-build-tvos-device', toggle_false('build_tvos_device'),
           help='skip building Swift stdlibs for tvOS devices '
                '(i.e. build simulators only)')
    option('--skip-build-tvos-simulator', toggle_false('build_tvos_simulator'),
           help='skip building Swift stdlibs for tvOS simulator '
                '(i.e. build devices only)')

    option('--skip-build-watchos', toggle_false('build_watchos'),
           help='skip building Swift stdlibs for watchOS')
    option('--skip-build-watchos-device', toggle_false('build_watchos_device'),
           help='skip building Swift stdlibs for watchOS devices '
                '(i.e. build simulators only)')
    option('--skip-build-watchos-simulator',
           toggle_false('build_watchos_simulator'),
           help='skip building Swift stdlibs for watchOS simulator '
                '(i.e. build devices only)')

    option('--skip-build-android', toggle_false('build_android'),
           help='skip building Swift stdlibs for Android')

    option('--skip-build-benchmarks', toggle_false('build_benchmarks'),
           help='skip building Swift Benchmark Suite')

    option('--build-external-benchmarks', toggle_true,
           help='skip building Swift Benchmark Suite')

    # -------------------------------------------------------------------------
    in_group('Skip testing specified targets')

    option('--skip-test-ios',
           toggle_false('test_ios'),
           help='skip testing all iOS targets. Equivalent to specifying both '
                '--skip-test-ios-simulator and --skip-test-ios-host')
    option('--skip-test-ios-simulator',
           toggle_false('test_ios_simulator'),
           help='skip testing iOS simulator targets')
    option('--skip-test-ios-32bit-simulator',
           toggle_false('test_ios_32bit_simulator'),
           help='skip testing iOS 32 bit simulator targets')
    option('--skip-test-ios-host',
           toggle_false('test_ios_host'),
           help='skip testing iOS device targets on the host machine (the '
                'phone itself)')

    option('--skip-test-tvos',
           toggle_false('test_tvos'),
           help='skip testing all tvOS targets. Equivalent to specifying both '
                '--skip-test-tvos-simulator and --skip-test-tvos-host')
    option('--skip-test-tvos-simulator',
           toggle_false('test_tvos_simulator'),
           help='skip testing tvOS simulator targets')
    option('--skip-test-tvos-host',
           toggle_false('test_tvos_host'),
           help='skip testing tvOS device targets on the host machine (the '
                'TV itself)')

    option('--skip-test-watchos',
           toggle_false('test_watchos'),
           help='skip testing all tvOS targets. Equivalent to specifying both '
                '--skip-test-watchos-simulator and --skip-test-watchos-host')
    option('--skip-test-watchos-simulator',
           toggle_false('test_watchos_simulator'),
           help='skip testing watchOS simulator targets')
    option('--skip-test-watchos-host',
           toggle_false('test_watchos_host'),
           help='skip testing watchOS device targets on the host machine (the '
                'watch itself)')

    option('--skip-test-android-host',
           toggle_false('test_android_host'),
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
    in_group('Unsupported options')

    option('--build-jobs', unsupported)
    option('--common-cmake-options', unsupported)
    option('--only-execute', unsupported)
    option('--skip-test-optimize-for-size', unsupported)
    option('--skip-test-optimized', unsupported)

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
                     /swift-syntax               (optional, requires swiftpm)
                     /swift-stress-tester        (optional,
                                                   requires swift-syntax)
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
