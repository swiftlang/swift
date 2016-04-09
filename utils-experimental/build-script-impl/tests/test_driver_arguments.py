# tests/test_cmake.py -------------------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import unittest
import sys
try:
    # py2
    from StringIO import StringIO
except ImportError:
    # py3
    from io import StringIO

from build_script.driver_arguments import Args, create_argparser


class ArgsTestCase(unittest.TestCase):

    def test_args(self):
        args = Args()

        args.foo = 42
        setattr(args, 'bar', 'Swift')

        self.assertEqual(args.foo, 42)
        self.assertEqual(args.bar, 'Swift')

        del args.foo
        self.assertFalse(hasattr(args, 'foo'))

        # Immutable after freeze()
        Args.freeze(args)

        def _setattr1():
            args.foo = 42

        def _setattr2():
            setattr(args, 'foo', 42)

        def _delattr1():
            del args.bar

        self.assertRaises(AttributeError, _setattr1)
        self.assertRaises(AttributeError, _setattr2)
        self.assertRaises(AttributeError, _delattr1)
        self.assertEqual(args.bar, 'Swift')
        self.assertFalse(hasattr(args, 'foo'))

    # TODO: This test should be removed if we are merged to master.
    def test_parser_accepts_all_old_args(self):
        # Tests that we migrated all `build-script-impl` options.
        parser = create_argparser()

        orig_err = sys.stderr

        for arg in ALL_OLD_ARGS:
            err = StringIO()
            sys.stderr = err
            try:
                self.assertIsNotNone(
                    # ALL old args should accept one argument.
                    parser.parse_args(['--' + arg + "=1"]))
            except SystemExit:
                # invalid choice error. At least, we have declaration :)
                self.assertIn('invalid choice', err.getvalue())

        sys.stderr = orig_err


ALL_OLD_ARGS = [
    "build-args",
    "build-dir",
    "host-cc",
    "host-cxx",
    "darwin-xcrun-toolchain",
    "build-ninja",
    "cmark-build-type",
    "lldb-extra-cmake-args",
    "lldb-extra-xcodebuild-args",
    "lldb-test-cc",
    "lldb-test-with-curses",
    "lldb-no-debugserver",
    "lldb-use-system-debugserver",
    "llvm-build-type",
    "llvm-enable-assertions",
    "swift-build-type",
    "swift-enable-assertions",
    "swift-analyze-code-coverage",
    "swift-enable-lto",
    "llvm-enable-lto",
    "swift-stdlib-build-type",
    "swift-stdlib-enable-assertions",
    "swift-stdlib-enable-reflection-metadata",
    "swift-stdlib-enable-resilience",
    "swift-stdlib-sil-serialize-all",
    "lldb-build-type",
    "llbuild-build-type",
    "foundation-build-type",
    "llbuild-enable-assertions",
    "enable-asan",
    "enable-ubsan",
    "cmake",
    "distcc",
    "build-runtime-with-host-compiler",
    "user-config-args",
    "cmake-generator",
    "verbose-build",
    "install-prefix",
    "toolchain-prefix",
    "install-destdir",
    "install-symroot",
    "swift-install-components",
    "llvm-install-components",
    "installable-package",
    "test-installable-package",
    "reconfigure",
    "swift-sdks",
    "swift-primary-variant-sdk",
    "swift-primary-variant-arch",
    "skip-ios",
    "skip-tvos",
    "skip-watchos",
    "skip-build-cmark",
    "skip-build-llvm",
    "skip-build-swift",
    "skip-build-linux",
    "skip-build-freebsd",
    "skip-build-cygwin",
    "skip-build-osx",
    "skip-build-ios",
    "skip-build-ios-device",
    "skip-build-ios-simulator",
    "skip-build-tvos",
    "skip-build-tvos-device",
    "skip-build-tvos-simulator",
    "skip-build-watchos",
    "skip-build-watchos-device",
    "skip-build-watchos-simulator",
    "skip-build-android",
    "skip-build-lldb",
    "skip-build-llbuild",
    "skip-build-swiftpm",
    "skip-build-xctest",
    "skip-build-foundation",
    "skip-build-libdispatch",
    "skip-build-benchmarks",
    "skip-test-cmark",
    "skip-test-lldb",
    "skip-test-swift",
    "skip-test-llbuild",
    "skip-test-swiftpm",
    "skip-test-xctest",
    "skip-test-foundation",
    "skip-test-libdispatch",
    "skip-test-linux",
    "skip-test-freebsd",
    "skip-test-cygwin",
    "skip-test-osx",
    "skip-test-ios-simulator",
    "skip-test-ios-host",
    "skip-test-tvos-simulator",
    "skip-test-tvos-host",
    "skip-test-watchos-simulator",
    "skip-test-watchos-host",
    "skip-test-validation",
    "skip-test-benchmarks",
    "skip-test-optimized",
    "stress-test-sourcekit",
    "workspace",
    "enable-llvm-assertions",
    "build-llvm",
    "build-swift-tools",
    "build-swift-stdlib",
    "build-swift-stdlib-unittest-extra",
    "build-swift-sdk-overlay",
    "build-swift-static-stdlib",
    "build-swift-examples",
    "build-serialized-stdlib-unittest",
    "build-sil-debugging-stdlib",
    "source-tree-includes-tests",
    "native-llvm-tools-path",
    "native-clang-tools-path",
    "native-swift-tools-path",
    "compiler-vendor",
    "clang-user-visible-version",
    "swift-user-visible-version",
    "swift-compiler-version",
    "clang-compiler-version",
    "embed-bitcode-section",
    "darwin-crash-reporter-client",
    "darwin-stdlib-install-name-dir",
    "install-cmark",
    "install-swift",
    "install-lldb",
    "install-llbuild",
    "install-swiftpm",
    "install-xctest",
    "install-foundation",
    "install-libdispatch",
    "darwin-install-extract-symbols",
    "host-target",
    "stdlib-deployment-targets",
    "cross-compile-tools-deployment-targets",
    "skip-merge-lipo-cross-compile-tools",
    "darwin-deployment-version-osx",
    "darwin-deployment-version-ios",
    "darwin-deployment-version-tvos",
    "darwin-deployment-version-watchos",
    "extra-cmake-options",
    "extra-swift-args",
    "sil-verify-all",
    "swift-enable-ast-verifier",
    "swift-runtime-enable-leak-checker",
    "use-gold-linker",
    "darwin-toolchain-bundle-identifier",
    "darwin-toolchain-display-name",
    "darwin-toolchain-name",
    "darwin-toolchain-version",
    "darwin-toolchain-application-cert",
    "darwin-toolchain-installer-cert",
    "darwin-toolchain-installer-package",
    "build-jobs",
    "darwin-toolchain-alias",
    "android-ndk",
    "android-ndk-version",
    "android-ndk-toolchain-version",
    "android-icu-uc",
    "android-icu-uc-include",
    "android-icu-i18n",
    "android-icu-i18n-include",
    "export-compile-commands",
]
