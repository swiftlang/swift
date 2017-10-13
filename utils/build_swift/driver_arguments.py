# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import argparse
import multiprocessing

import android.adb.commands

from swift_build_support.swift_build_support import arguments
from swift_build_support.swift_build_support import host
from swift_build_support.swift_build_support import targets

from swift_build_support.swift_build_support.targets import \
    StdlibDeploymentTarget

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
        args.build_variant = "Debug"

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
        args.cmake_generator = "Ninja"

    # --ios-all etc are not supported by open-source Swift.
    if args.ios_all:
        raise ValueError("error: --ios-all is unavailable in open-source "
                         "Swift.\nUse --ios to skip iOS device tests.")

    if args.tvos_all:
        raise ValueError("error: --tvos-all is unavailable in open-source "
                         "Swift.\nUse --tvos to skip tvOS device tests.")

    if args.watchos_all:
        raise ValueError("error: --watchos-all is unavailable in open-source "
                         "Swift.\nUse --watchos to skip watchOS device tests.")

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
        args.build_playgroundlogger = False
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

    parser = _ApplyDefaultsArgumentParser(
        apply_defaults=_apply_default_arguments,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        usage=USAGE,
        description=DESCRIPTION,
        epilog=EPILOG)

    parser.add_argument(
        "-n", "--dry-run",
        help="print the commands that would be executed, but do not execute "
             "them",
        action="store_true",
        default=False)
    parser.add_argument(
        "--no-legacy-impl", dest="legacy_impl",
        help="avoid legacy implementation",
        action="store_false",
        default=True)

    targets_group = parser.add_argument_group(
        title="Host and cross-compilation targets")
    targets_group.add_argument(
        "--host-target",
        help="The host target. LLVM, Clang, and Swift will be built for this "
             "target. The built LLVM and Clang will be used to compile Swift "
             "for the cross-compilation targets.",
        default=StdlibDeploymentTarget.host_target().name)
    targets_group.add_argument(
        "--cross-compile-hosts",
        help="A space separated list of targets to cross-compile host Swift "
             "tools for. Can be used multiple times.",
        action=arguments.action.concat, type=arguments.type.shell_split,
        default=[])
    targets_group.add_argument(
        "--stdlib-deployment-targets",
        help="list of targets to compile or cross-compile the Swift standard "
             "library for. %(default)s by default.",
        action=arguments.action.concat, type=arguments.type.shell_split,
        default=None)
    targets_group.add_argument(
        "--build-stdlib-deployment-targets",
        help="A space-separated list that filters which of the configured "
             "targets to build the Swift standard library for, or 'all'.",
        type=arguments.type.shell_split, default=["all"])

    projects_group = parser.add_argument_group(
        title="Options to select projects")
    projects_group.add_argument(
        "-l", "--lldb",
        help="build LLDB",
        action="store_true",
        dest="build_lldb")
    projects_group.add_argument(
        "-b", "--llbuild",
        help="build llbuild",
        action="store_true",
        dest="build_llbuild")
    projects_group.add_argument(
        "-p", "--swiftpm",
        help="build swiftpm",
        action="store_true",
        dest="build_swiftpm")
    projects_group.add_argument(
        "--xctest",
        help="build xctest",
        action=arguments.action.enable,
        dest="build_xctest")
    projects_group.add_argument(
        "--foundation",
        help="build foundation",
        action=arguments.action.enable,
        dest="build_foundation")
    projects_group.add_argument(
        "--libdispatch",
        help="build libdispatch",
        action=arguments.action.enable,
        dest="build_libdispatch")
    projects_group.add_argument(
        "--libicu",
        help="build libicu",
        action=arguments.action.enable,
        dest="build_libicu")
    projects_group.add_argument(
        "--playgroundlogger",
        help="build playgroundlogger",
        action="store_true",
        dest="build_playgroundlogger")
    projects_group.add_argument(
        "--playgroundsupport",
        help="build PlaygroundSupport",
        action="store_true",
        dest="build_playgroundsupport")
    projects_group.add_argument(
        "--build-ninja",
        help="build the Ninja tool",
        action=arguments.action.enable)

    extra_actions_group = parser.add_argument_group(
        title="Extra actions to perform before or in addition to building")
    extra_actions_group.add_argument(
        "-c", "--clean",
        help="do a clean build",
        action="store_true")
    extra_actions_group.add_argument(
        "--export-compile-commands",
        help="generate compilation databases in addition to building",
        action=arguments.action.enable)
    extra_actions_group.add_argument(
        "--symbols-package",
        metavar="PATH",
        help="if provided, an archive of the symbols directory will be "
             "generated at this path")

    build_variant_group = parser.add_mutually_exclusive_group(required=False)
    build_variant_group.add_argument(
        "-d", "--debug",
        help="build the Debug variant of everything (LLVM, Clang, Swift host "
             "tools, target Swift standard libraries, LLDB (if enabled) "
             "(default)",
        action="store_const",
        const="Debug",
        dest="build_variant")
    build_variant_group.add_argument(
        "-r", "--release-debuginfo",
        help="build the RelWithDebInfo variant of everything (default is "
             "Debug)",
        action="store_const",
        const="RelWithDebInfo",
        dest="build_variant")
    build_variant_group.add_argument(
        "-R", "--release",
        help="build the Release variant of everything (default is Debug)",
        action="store_const",
        const="Release",
        dest="build_variant")

    build_variant_override_group = parser.add_argument_group(
        title="Override build variant for a specific project")
    build_variant_override_group.add_argument(
        "--debug-llvm",
        help="build the Debug variant of LLVM",
        action="store_const",
        const="Debug",
        dest="llvm_build_variant")
    build_variant_override_group.add_argument(
        "--debug-swift",
        help="build the Debug variant of Swift host tools",
        action="store_const",
        const="Debug",
        dest="swift_build_variant")
    build_variant_override_group.add_argument(
        "--debug-swift-stdlib",
        help="build the Debug variant of the Swift standard library and SDK "
             "overlay",
        action="store_const",
        const="Debug",
        dest="swift_stdlib_build_variant")
    build_variant_override_group.add_argument(
        "--debug-lldb",
        help="build the Debug variant of LLDB",
        action="store_const",
        const="Debug",
        dest="lldb_build_variant")
    build_variant_override_group.add_argument(
        "--debug-cmark",
        help="build the Debug variant of CommonMark",
        action="store_const",
        const="Debug",
        dest="cmark_build_variant")
    build_variant_override_group.add_argument(
        "--debug-foundation",
        help="build the Debug variant of Foundation",
        action="store_const",
        const="Debug",
        dest="foundation_build_variant")
    build_variant_override_group.add_argument(
        "--debug-libdispatch",
        help="build the Debug variant of libdispatch",
        action="store_const",
        const="Debug",
        dest="libdispatch_build_variant")
    build_variant_override_group.add_argument(
        "--debug-libicu",
        help="build the Debug variant of libicu",
        action="store_const",
        const="Debug",
        dest="libicu_build_variant")

    assertions_group = parser.add_mutually_exclusive_group(required=False)
    assertions_group.add_argument(
        "--assertions",
        help="enable assertions in all projects",
        action="store_const",
        const=True,
        dest="assertions")
    assertions_group.add_argument(
        "--no-assertions",
        help="disable assertions in all projects",
        action="store_const",
        const=False,
        dest="assertions")

    assertions_override_group = parser.add_argument_group(
        title="Control assertions in a specific project")
    assertions_override_group.add_argument(
        "--cmark-assertions",
        help="enable assertions in CommonMark",
        action="store_const",
        const=True,
        dest="cmark_assertions")
    assertions_override_group.add_argument(
        "--llvm-assertions",
        help="enable assertions in LLVM",
        action="store_const",
        const=True,
        dest="llvm_assertions")
    assertions_override_group.add_argument(
        "--no-llvm-assertions",
        help="disable assertions in LLVM",
        action="store_const",
        const=False,
        dest="llvm_assertions")
    assertions_override_group.add_argument(
        "--swift-assertions",
        help="enable assertions in Swift",
        action="store_const",
        const=True,
        dest="swift_assertions")
    assertions_override_group.add_argument(
        "--no-swift-assertions",
        help="disable assertions in Swift",
        action="store_const",
        const=False,
        dest="swift_assertions")
    assertions_override_group.add_argument(
        "--swift-stdlib-assertions",
        help="enable assertions in the Swift standard library",
        action="store_const",
        const=True,
        dest="swift_stdlib_assertions")
    assertions_override_group.add_argument(
        "--no-swift-stdlib-assertions",
        help="disable assertions in the Swift standard library",
        action="store_const",
        const=False,
        dest="swift_stdlib_assertions")
    assertions_override_group.add_argument(
        "--lldb-assertions",
        help="enable assertions in LLDB",
        action="store_const",
        const=True,
        dest="lldb_assertions")
    assertions_override_group.add_argument(
        "--no-lldb-assertions",
        help="disable assertions in LLDB",
        action="store_const",
        const=False,
        dest="lldb_assertions")

    # FIXME: This should be one option using choices=[...]
    cmake_generator_group = parser.add_argument_group(
        title="Select the CMake generator")
    cmake_generator_group.add_argument(
        "-x", "--xcode",
        help="use CMake's Xcode generator (default is Ninja)",
        action="store_const",
        const="Xcode",
        dest="cmake_generator")
    cmake_generator_group.add_argument(
        "-m", "--make",
        help="use CMake's Makefile generator (default is Ninja)",
        action="store_const",
        const="Unix Makefiles",
        dest="cmake_generator")
    cmake_generator_group.add_argument(
        "-e", "--eclipse",
        help="use CMake's Eclipse generator (default is Ninja)",
        action="store_const",
        const="Eclipse CDT4 - Ninja",
        dest="cmake_generator")

    run_tests_group = parser.add_argument_group(
        title="Run tests")

    # NOTE: We can't merge -t and --test, because nargs='?' makes
    #       `-ti` to be treated as `-t=i`.
    run_tests_group.add_argument(
        "-t",
        help="test Swift after building",
        action="store_const",
        const=True,
        dest="test")
    run_tests_group.add_argument(
        "--test",
        help="test Swift after building",
        action=arguments.action.enable)
    run_tests_group.add_argument(
        "-T",
        help="run the validation test suite (implies --test)",
        action="store_const",
        const=True,
        dest="validation_test")
    run_tests_group.add_argument(
        "--validation-test",
        help="run the validation test suite (implies --test)",
        action=arguments.action.enable)
    run_tests_group.add_argument(
        "--test-paths",
        help="run tests located in specific directories and/or files \
        (implies --test and/or --validation-test)",
        action=arguments.action.concat, type=arguments.type.shell_split,
        default=[])
    run_tests_group.add_argument(
        "-o",
        help="run the test suite in optimized mode too (implies --test)",
        action="store_const",
        const=True,
        dest="test_optimized")
    run_tests_group.add_argument(
        "--test-optimized",
        help="run the test suite in optimized mode too (implies --test)",
        action=arguments.action.enable)
    run_tests_group.add_argument(
        "-s",
        help="run the test suite in optimize for size mode too \
        (implies --test)",
        action="store_const",
        const=True,
        dest="test_optimize_for_size")
    run_tests_group.add_argument(
        "--test-optimize-for-size",
        help="run the test suite in optimize for size mode too \
        (implies --test)",
        action=arguments.action.enable)
    run_tests_group.add_argument(
        "--long-test",
        help="run the long test suite",
        action=arguments.action.enable)
    run_tests_group.add_argument(
        "--host-test",
        help="run executable tests on host devices (such as iOS or tvOS)",
        action=arguments.action.enable)
    run_tests_group.add_argument(
        "-B", "--benchmark",
        help="run the Swift Benchmark Suite after building",
        action="store_true")
    run_tests_group.add_argument(
        "--benchmark-num-o-iterations",
        help="if the Swift Benchmark Suite is run after building, run N \
iterations with -O",
        metavar='N', type=int, default=3)
    run_tests_group.add_argument(
        "--benchmark-num-onone-iterations",
        help="if the Swift Benchmark Suite is run after building, run N \
        iterations with -Onone", metavar='N', type=int, default=3)
    run_tests_group.add_argument(
        "--skip-test-osx",
        dest='test_osx',
        action=arguments.action.disable,
        help="skip testing Swift stdlibs for Mac OS X")
    run_tests_group.add_argument(
        "--skip-test-linux",
        dest='test_linux',
        action=arguments.action.disable,
        help="skip testing Swift stdlibs for Linux")
    run_tests_group.add_argument(
        "--skip-test-freebsd",
        dest='test_freebsd',
        action=arguments.action.disable,
        help="skip testing Swift stdlibs for FreeBSD")
    run_tests_group.add_argument(
        "--skip-test-cygwin",
        dest='test_cygwin',
        action=arguments.action.disable,
        help="skip testing Swift stdlibs for Cygwin")
    parser.add_argument(
        "--build-runtime-with-host-compiler",
        help="Use the host compiler, not the self-built one to compile the "
             "Swift runtime",
        action=arguments.action.enable)

    run_build_group = parser.add_argument_group(
        title="Run build")
    run_build_group.add_argument(
        "--build-swift-dynamic-stdlib",
        help="build dynamic variants of the Swift standard library",
        action=arguments.action.enable,
        default=True)
    run_build_group.add_argument(
        "--build-swift-static-stdlib",
        help="build static variants of the Swift standard library",
        action=arguments.action.enable)
    run_build_group.add_argument(
        "--build-swift-dynamic-sdk-overlay",
        help="build dynamic variants of the Swift SDK overlay",
        action=arguments.action.enable,
        default=True)
    run_build_group.add_argument(
        "--build-swift-static-sdk-overlay",
        help="build static variants of the Swift SDK overlay",
        action=arguments.action.enable)
    run_build_group.add_argument(
        "--build-swift-stdlib-unittest-extra",
        help="Build optional StdlibUnittest components",
        action=arguments.action.enable)
    run_build_group.add_argument(
        "-S", "--skip-build",
        help="generate build directory only without building",
        action="store_true")
    run_build_group.add_argument(
        "--skip-build-linux",
        dest='build_linux',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for Linux")
    run_build_group.add_argument(
        "--skip-build-freebsd",
        dest='build_freebsd',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for FreeBSD")
    run_build_group.add_argument(
        "--skip-build-cygwin",
        dest='build_cygwin',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for Cygwin")
    run_build_group.add_argument(
        "--skip-build-osx",
        dest='build_osx',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for MacOSX")

    run_build_group.add_argument(
        "--skip-build-ios",
        dest='build_ios',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for iOS")
    run_build_group.add_argument(
        "--skip-build-ios-device",
        dest='build_ios_device',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for iOS devices "
             "(i.e. build simulators only)")
    run_build_group.add_argument(
        "--skip-build-ios-simulator",
        dest='build_ios_simulator',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for iOS simulator "
             "(i.e. build devices only)")

    run_build_group.add_argument(
        "--skip-build-tvos",
        dest='build_tvos',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for tvOS")
    run_build_group.add_argument(
        "--skip-build-tvos-device",
        dest='build_tvos_device',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for tvOS devices "
             "(i.e. build simulators only)")
    run_build_group.add_argument(
        "--skip-build-tvos-simulator",
        dest='build_tvos_simulator',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for tvOS simulator "
             "(i.e. build devices only)")

    run_build_group.add_argument(
        "--skip-build-watchos",
        dest='build_watchos',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for watchOS")
    run_build_group.add_argument(
        "--skip-build-watchos-device",
        dest='build_watchos_device',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for watchOS devices "
             "(i.e. build simulators only)")
    run_build_group.add_argument(
        "--skip-build-watchos-simulator",
        dest='build_watchos_simulator',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for watchOS simulator "
             "(i.e. build devices only)")

    run_build_group.add_argument(
        "--skip-build-android",
        dest='build_android',
        action=arguments.action.disable,
        help="skip building Swift stdlibs for Android")

    run_build_group.add_argument(
        "--skip-build-benchmarks",
        dest='build_benchmarks',
        action=arguments.action.disable,
        help="skip building Swift Benchmark Suite")

    run_build_group.add_argument(
        "--build-external-benchmarks",
        dest='build_external_benchmarks',
        action=arguments.action.enable,
        help="skip building Swift Benchmark Suite")

    skip_test_group = parser.add_argument_group(
        title="Skip testing specified targets")
    skip_test_group.add_argument(
        "--skip-test-ios",
        dest='test_ios',
        action=arguments.action.disable,
        help="skip testing all iOS targets. Equivalent to specifying both "
             "--skip-test-ios-simulator and --skip-test-ios-host")
    skip_test_group.add_argument(
        "--skip-test-ios-simulator",
        dest='test_ios_simulator',
        action=arguments.action.disable,
        help="skip testing iOS simulator targets")
    skip_test_group.add_argument(
        "--skip-test-ios-32bit-simulator",
        dest='test_ios_32bit_simulator',
        action=arguments.action.disable,
        help="skip testing iOS 32 bit simulator targets")
    skip_test_group.add_argument(
        "--skip-test-ios-host",
        dest='test_ios_host',
        action=arguments.action.disable,
        help="skip testing iOS device targets on the host machine (the phone "
             "itself)")
    skip_test_group.add_argument(
        "--skip-test-tvos",
        dest='test_tvos',
        action=arguments.action.disable,
        help="skip testing all tvOS targets. Equivalent to specifying both "
             "--skip-test-tvos-simulator and --skip-test-tvos-host")
    skip_test_group.add_argument(
        "--skip-test-tvos-simulator",
        dest='test_tvos_simulator',
        action=arguments.action.disable,
        help="skip testing tvOS simulator targets")
    skip_test_group.add_argument(
        "--skip-test-tvos-host",
        dest='test_tvos_host',
        action=arguments.action.disable,
        help="skip testing tvOS device targets on the host machine (the TV "
             "itself)")
    skip_test_group.add_argument(
        "--skip-test-watchos",
        dest='test_watchos',
        action=arguments.action.disable,
        help="skip testing all tvOS targets. Equivalent to specifying both "
             "--skip-test-watchos-simulator and --skip-test-watchos-host")
    skip_test_group.add_argument(
        "--skip-test-watchos-simulator",
        dest='test_watchos_simulator',
        action=arguments.action.disable,
        help="skip testing watchOS simulator targets")
    skip_test_group.add_argument(
        "--skip-test-watchos-host",
        dest='test_watchos_host',
        action=arguments.action.disable,
        help="skip testing watchOS device targets on the host machine (the "
             "watch itself)")
    skip_test_group.add_argument(
        "--skip-test-android-host",
        dest='test_android_host',
        action=arguments.action.disable,
        help="skip testing Android device targets on the host machine (the "
             "phone itself)")

    parser.add_argument(
        "-i", "--ios",
        help="also build for iOS, but disallow tests that require an iOS "
             "device",
        action="store_true")
    parser.add_argument(
        "-I", "--ios-all",
        help="also build for iOS, and allow all iOS tests",
        action="store_true",
        dest="ios_all")
    parser.add_argument(
        "--skip-ios",
        help="set to skip everything iOS-related",
        dest="ios",
        action="store_false")

    parser.add_argument(
        "--tvos",
        help="also build for tvOS, but disallow tests that require a tvos "
             "device",
        action=arguments.action.enable)
    parser.add_argument(
        "--tvos-all",
        help="also build for tvOS, and allow all tvOS tests",
        action=arguments.action.enable,
        dest="tvos_all")
    parser.add_argument(
        "--skip-tvos",
        help="set to skip everything tvOS-related",
        dest="tvos",
        action="store_false")

    parser.add_argument(
        "--watchos",
        help="also build for watchOS, but disallow tests that require an "
             "watchOS device",
        action=arguments.action.enable)
    parser.add_argument(
        "--watchos-all",
        help="also build for Apple watchOS, and allow all Apple watchOS tests",
        action=arguments.action.enable,
        dest="watchos_all")
    parser.add_argument(
        "--skip-watchos",
        help="set to skip everything watchOS-related",
        dest="watchos",
        action="store_false")

    parser.add_argument(
        "--android",
        help="also build for Android",
        action=arguments.action.enable)

    parser.add_argument(
        "--swift-analyze-code-coverage",
        help="enable code coverage analysis in Swift (false, not-merged, "
             "merged).",
        choices=["false", "not-merged", "merged"],
        # so CMake can see the inert mode as a false value
        default=defaults.SWIFT_ANALYZE_CODE_COVERAGE,
        dest="swift_analyze_code_coverage")

    parser.add_argument(
        "--build-subdir",
        help="name of the directory under $SWIFT_BUILD_ROOT where the build "
             "products will be placed",
        metavar="PATH")
    parser.add_argument(
        "--install-prefix",
        help="The installation prefix. This is where built Swift products "
             "(like bin, lib, and include) will be installed.",
        metavar="PATH",
        default=targets.install_prefix())
    parser.add_argument(
        "--install-symroot",
        help="the path to install debug symbols into",
        metavar="PATH")

    parser.add_argument(
        "-j", "--jobs",
        help="the number of parallel build jobs to use",
        type=int,
        dest="build_jobs",
        default=multiprocessing.cpu_count())

    parser.add_argument(
        "--darwin-xcrun-toolchain",
        help="the name of the toolchain to use on Darwin",
        default=defaults.DARWIN_XCRUN_TOOLCHAIN)
    parser.add_argument(
        "--cmake",
        help="the path to a CMake executable that will be used to build "
             "Swift",
        type=arguments.type.executable,
        metavar="PATH")
    parser.add_argument(
        "--show-sdks",
        help="print installed Xcode and SDK versions",
        action=arguments.action.enable)

    parser.add_argument(
        "--extra-swift-args",
        help="Pass through extra flags to swift in the form of a cmake list "
             "'module_regexp;flag'. Can be called multiple times to add "
             "multiple such module_regexp flag pairs. All semicolons in flags "
             "must be escaped with a '\\'",
        action="append", dest="extra_swift_args", default=[])

    llvm_group = parser.add_argument_group(
        title="Build settings specific for LLVM")
    llvm_group.add_argument(
        '--llvm-targets-to-build',
        help='LLVM target generators to build',
        default="X86;ARM;AArch64;PowerPC;SystemZ;Mips")

    android_group = parser.add_argument_group(
        title="Build settings for Android")
    android_group.add_argument(
        "--android-ndk",
        help="An absolute path to the NDK that will be used as a libc "
             "implementation for Android builds",
        metavar="PATH")
    android_group.add_argument(
        "--android-api-level",
        help="The Android API level to target when building for Android. "
             "Currently only 21 or above is supported",
        default="21")
    android_group.add_argument(
        "--android-ndk-gcc-version",
        help="The GCC version to use when building for Android. Currently "
             "only 4.9 is supported. %(default)s is also the default value. "
             "This option may be used when experimenting with versions "
             "of the Android NDK not officially supported by Swift",
        choices=["4.8", "4.9"],
        default="4.9")
    android_group.add_argument(
        "--android-icu-uc",
        help="Path to a directory containing libicuuc.so",
        metavar="PATH")
    android_group.add_argument(
        "--android-icu-uc-include",
        help="Path to a directory containing headers for libicuuc",
        metavar="PATH")
    android_group.add_argument(
        "--android-icu-i18n",
        help="Path to a directory containing libicui18n.so",
        metavar="PATH")
    android_group.add_argument(
        "--android-icu-i18n-include",
        help="Path to a directory containing headers libicui18n",
        metavar="PATH")
    android_group.add_argument(
        "--android-deploy-device-path",
        help="Path on an Android device to which built Swift stdlib products "
             "will be deployed. If running host tests, specify the '{}' "
             "directory.".format(android.adb.commands.DEVICE_TEMP_DIR),
        default=android.adb.commands.DEVICE_TEMP_DIR,
        metavar="PATH")

    parser.add_argument(
        "--host-cc",
        help="the absolute path to CC, the 'clang' compiler for the host "
             "platform. Default is auto detected.",
        type=arguments.type.executable,
        metavar="PATH")
    parser.add_argument(
        "--host-cxx",
        help="the absolute path to CXX, the 'clang++' compiler for the host "
             "platform. Default is auto detected.",
        type=arguments.type.executable,
        metavar="PATH")
    parser.add_argument(
        "--host-lipo",
        help="the absolute path to lipo. Default is auto detected.",
        type=arguments.type.executable,
        metavar="PATH")
    parser.add_argument(
        "--host-libtool",
        help="the absolute path to libtool. Default is auto detected.",
        type=arguments.type.executable,
        metavar="PATH")
    parser.add_argument(
        "--distcc",
        help="use distcc in pump mode",
        action=arguments.action.enable)
    parser.add_argument(
        "--enable-asan",
        help="enable Address Sanitizer",
        action=arguments.action.enable)
    parser.add_argument(
        "--enable-ubsan",
        help="enable Undefined Behavior Sanitizer",
        action=arguments.action.enable)
    parser.add_argument(
        "--enable-tsan",
        help="enable Thread Sanitizer for swift tools",
        action=arguments.action.enable)
    parser.add_argument(
        "--enable-tsan-runtime",
        help="enable Thread Sanitizer on the swift runtime")
    parser.add_argument(
        "--enable-lsan",
        help="enable Leak Sanitizer for swift tools",
        action=arguments.action.enable)

    parser.add_argument(
        "--compiler-vendor",
        choices=["none", "apple"],
        default=defaults.COMPILER_VENDOR,
        help="Compiler vendor name")
    parser.add_argument(
        "--clang-compiler-version",
        help="string that indicates a compiler version for Clang",
        type=arguments.type.clang_compiler_version,
        metavar="MAJOR.MINOR.PATCH")
    parser.add_argument(
        "--clang-user-visible-version",
        help="User-visible version of the embedded Clang and LLVM compilers",
        type=arguments.type.clang_compiler_version,
        default=defaults.CLANG_USER_VISIBLE_VERSION,
        metavar="MAJOR.MINOR.PATCH")
    parser.add_argument(
        "--swift-compiler-version",
        help="string that indicates a compiler version for Swift",
        type=arguments.type.swift_compiler_version,
        metavar="MAJOR.MINOR")
    parser.add_argument(
        "--swift-user-visible-version",
        help="User-visible version of the embedded Swift compiler",
        type=arguments.type.swift_compiler_version,
        default=defaults.SWIFT_USER_VISIBLE_VERSION,
        metavar="MAJOR.MINOR")

    parser.add_argument(
        "--darwin-deployment-version-osx",
        help="minimum deployment target version for OS X",
        metavar="MAJOR.MINOR",
        default=defaults.DARWIN_DEPLOYMENT_VERSION_OSX)
    parser.add_argument(
        "--darwin-deployment-version-ios",
        help="minimum deployment target version for iOS",
        metavar="MAJOR.MINOR",
        default=defaults.DARWIN_DEPLOYMENT_VERSION_IOS)
    parser.add_argument(
        "--darwin-deployment-version-tvos",
        help="minimum deployment target version for tvOS",
        metavar="MAJOR.MINOR",
        default=defaults.DARWIN_DEPLOYMENT_VERSION_TVOS)
    parser.add_argument(
        "--darwin-deployment-version-watchos",
        help="minimum deployment target version for watchOS",
        metavar="MAJOR.MINOR",
        default=defaults.DARWIN_DEPLOYMENT_VERSION_WATCHOS)

    parser.add_argument(
        "--extra-cmake-options",
        help="Pass through extra options to CMake in the form of comma "
             "separated options '-DCMAKE_VAR1=YES,-DCMAKE_VAR2=/tmp'. Can be "
             "called multiple times to add multiple such options.",
        action=arguments.action.concat,
        type=arguments.type.shell_split,
        default=[])

    parser.add_argument(
        "--build-args",
        help="arguments to the build tool. This would be prepended to the "
             "default argument that is '-j8' when CMake generator is "
             "\"Ninja\".",
        type=arguments.type.shell_split,
        default=[])

    parser.add_argument(
        "--verbose-build",
        help="print the commands executed during the build",
        action=arguments.action.enable)

    parser.add_argument(
        "--lto",
        help="use lto optimization on llvm/swift tools. This does not "
             "imply using lto on the swift standard library or runtime. "
             "Options: thin, full. If no optional arg is provided, full is "
             "chosen by default",
        metavar="LTO_TYPE",
        nargs='?',
        choices=['thin', 'full'],
        default=None,
        const='full',
        dest='lto_type')

    parser.add_argument(
        "--clang-profile-instr-use",
        help="profile file to use for clang PGO",
        metavar="PATH")

    default_max_lto_link_job_counts = host.max_lto_link_job_counts()
    parser.add_argument(
        "--llvm-max-parallel-lto-link-jobs",
        help="the maximum number of parallel link jobs to use when compiling "
             "llvm",
        metavar="COUNT",
        default=default_max_lto_link_job_counts['llvm'])

    parser.add_argument(
        "--swift-tools-max-parallel-lto-link-jobs",
        help="the maximum number of parallel link jobs to use when compiling "
             "swift tools.",
        metavar="COUNT",
        default=default_max_lto_link_job_counts['swift'])

    parser.add_argument("--enable-sil-ownership",
                        help="Enable the SIL ownership model",
                        action='store_true')

    parser.add_argument("--force-optimized-typechecker",
                        help="Force the type checker to be built with "
                        "optimization",
                        action='store_true')

    parser.add_argument(
        # Explicitly unavailable options here.
        "--build-jobs",
        "--common-cmake-options",
        "--only-execute",
        "--skip-test-optimize-for-size",
        "--skip-test-optimized",
        action=arguments.action.unavailable)

    parser.add_argument(
        "--lit-args",
        help="lit args to use when testing",
        metavar="LITARGS",
        default="-sv")

    parser.add_argument(
        "--coverage-db",
        help="coverage database to use when prioritizing testing",
        metavar="PATH")

    return parser


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
