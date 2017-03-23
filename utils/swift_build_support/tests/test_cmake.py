# test_cmake.py - Unit tests for swift_build_support.cmake -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import unittest
from argparse import Namespace

from swift_build_support.arguments import CompilerVersion
from swift_build_support.cmake import CMake, CMakeOptions
from swift_build_support.toolchain import host_toolchain


class CMakeTestCase(unittest.TestCase):

    def mock_distcc_path(self):
        """Return a path string of mock distcc executable
        """
        return os.path.join(os.path.dirname(__file__),
                            'mock-distcc')

    def default_args(self):
        """Return new args object with default values
        """
        return Namespace(host_cc="/path/to/clang",
                         host_cxx="/path/to/clang++",
                         enable_asan=False,
                         enable_ubsan=False,
                         enable_tsan=False,
                         export_compile_commands=False,
                         distcc=False,
                         cmake_generator="Ninja",
                         clang_compiler_version=None,
                         build_jobs=8,
                         build_args=[],
                         verbose_build=False,
                         build_ninja=False)

    def which_ninja(self, args):
        toolchain = host_toolchain()
        if toolchain.ninja is not None:
            return '/path/to/installed/ninja'
        # Maybe we'll build a ninja, maybe we wont.
        # Fake it anyway for the tests.
        return '/path/to/built/ninja'

    def cmake(self, args):
        """Return new CMake object initialized with given args
        """
        toolchain = host_toolchain()
        toolchain.cc = args.host_cc
        toolchain.cxx = args.host_cxx
        if args.distcc:
            toolchain.distcc = self.mock_distcc_path()
        toolchain.ninja = self.which_ninja(args)
        return CMake(args=args, toolchain=toolchain)

    def test_common_options_defaults(self):
        args = self.default_args()
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_asan(self):
        args = self.default_args()
        args.enable_asan = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DLLVM_USE_SANITIZER=Address",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_ubsan(self):
        args = self.default_args()
        args.enable_ubsan = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DLLVM_USE_SANITIZER=Undefined",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_tsan(self):
        args = self.default_args()
        args.enable_tsan = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DLLVM_USE_SANITIZER=Thread",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_asan_ubsan(self):
        args = self.default_args()
        args.enable_asan = True
        args.enable_ubsan = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DLLVM_USE_SANITIZER=Address;Undefined",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_ubsan_tsan(self):
        args = self.default_args()
        args.enable_ubsan = True
        args.enable_tsan = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DLLVM_USE_SANITIZER=Undefined;Thread",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_asan_ubsan_tsan(self):
        args = self.default_args()
        args.enable_asan = True
        args.enable_ubsan = True
        args.enable_tsan = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DLLVM_USE_SANITIZER=Address;Undefined;Thread",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_export_compile_commands(self):
        args = self.default_args()
        args.export_compile_commands = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_distcc(self):
        args = self.default_args()
        args.distcc = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DCMAKE_C_COMPILER:PATH=" + self.mock_distcc_path(),
             "-DCMAKE_C_COMPILER_ARG1=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=" + self.mock_distcc_path(),
             "-DCMAKE_CXX_COMPILER_ARG1=/path/to/clang++",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_xcode(self):
        args = self.default_args()
        args.cmake_generator = 'Xcode'
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Xcode",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_CONFIGURATION_TYPES=" +
             "Debug;Release;MinSizeRel;RelWithDebInfo"])

    def test_common_options_clang_compiler_version(self):
        args = self.default_args()
        args.clang_compiler_version = CompilerVersion(
            string_representation="3.8.0",
            components=("3", "8", "0", None))
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DLLVM_VERSION_MAJOR:STRING=3",
             "-DLLVM_VERSION_MINOR:STRING=8",
             "-DLLVM_VERSION_PATCH:STRING=0",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_build_ninja(self):
        args = self.default_args()
        args.build_ninja = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_full(self):
        args = self.default_args()
        args.enable_asan = True
        args.enable_ubsan = True
        args.export_compile_commands = True
        args.distcc = True
        args.cmake_generator = 'Xcode'
        args.clang_compiler_version = CompilerVersion(
            string_representation="3.8.0",
            components=("3", "8", "0", None))
        args.build_ninja = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Xcode",
             "-DLLVM_USE_SANITIZER=Address;Undefined",
             "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
             "-DCMAKE_C_COMPILER:PATH=" + self.mock_distcc_path(),
             "-DCMAKE_C_COMPILER_ARG1=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=" + self.mock_distcc_path(),
             "-DCMAKE_CXX_COMPILER_ARG1=/path/to/clang++",
             "-DCMAKE_CONFIGURATION_TYPES=" +
             "Debug;Release;MinSizeRel;RelWithDebInfo",
             "-DLLVM_VERSION_MAJOR:STRING=3",
             "-DLLVM_VERSION_MINOR:STRING=8",
             "-DLLVM_VERSION_PATCH:STRING=0"])
        # NOTE: No "-DCMAKE_MAKE_PROGRAM=/path/to/built/ninja" because
        #       cmake_generator is 'Xcode'

    def test_build_args_ninja(self):
        args = self.default_args()
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.build_args()),
            ["-j8"])

        args.verbose_build = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.build_args()),
            ["-j8", "-v"])

    def test_build_args_makefile(self):
        args = self.default_args()
        args.cmake_generator = "Unix Makefiles"
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.build_args()),
            ["-j8"])

        args.verbose_build = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.build_args()),
            ["-j8", "VERBOSE=1"])

    def test_build_args_xcode(self):
        args = self.default_args()
        args.cmake_generator = "Xcode"
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.build_args()),
            ["-parallelizeTargets", "-jobs", "8"])

        # NOTE: Xcode generator DOES NOT take 'verbose-build' into account.
        args.verbose_build = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.build_args()),
            ["-parallelizeTargets", "-jobs", "8"])

    def test_build_args_eclipse_ninja(self):
        # NOTE: Eclipse generator DOES NOT take 'build-jobs' into account,
        #       nor 'verbose-build'.
        args = self.default_args()
        args.cmake_generator = "Eclipse CDT4 - Ninja"
        args.verbose_build = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.build_args()), [])

    def test_build_args_custom_build_args(self):
        args = self.default_args()
        args.build_args = ["-foo", "bar baz"]
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.build_args()),
            ["-foo", "bar baz", "-j8"])

    def test_build_args_distcc(self):
        args = self.default_args()
        args.distcc = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.build_args()),
            ["-j6"])


class CMakeOptionsTestCase(unittest.TestCase):
    def test_define(self):
        options = CMakeOptions()

        options.define('OPT1:STRING', 'foo')

        options.define('OPT2:BOOL', True)
        options.define('OPT3:BOOL', 1)
        options.define('OPT4:BOOL', 'True')
        options.define('OPT5:BOOL', 'true')
        options.define('OPT6:BOOL', 'YES')
        options.define('OPT7:BOOL', '1')

        options.define('OPT8:BOOL', False)
        options.define('OPT9:BOOL', 0)
        options.define('OPT10:BOOL', 'false')
        options.define('OPT11:BOOL', 'False')
        options.define('OPT12:BOOL', 'No')
        options.define('OPT13:BOOL', '0')

        options.define('OPT14', 12)
        options.define('OPT15', '')
        options.define('OPT16', None)
        options.define('OPT17:PATH', 'foo')

        self.assertRaises(ValueError, options.define, 'ERR', ["FOO"])
        self.assertRaises(ValueError, options.define, 'ERR', {"FOO": 1})

        self.assertRaises(ValueError, options.define, 'ERR:BOOL', None)
        self.assertRaises(ValueError, options.define, 'ERR:BOOL', 3)
        self.assertRaises(ValueError, options.define, 'ERR:BOOL', 'foo')
        self.assertRaises(ValueError, options.define, 'ERR:BOOL', [1])

        self.assertEqual(list(options), [
            '-DOPT1:STRING=foo',
            '-DOPT2:BOOL=TRUE',
            '-DOPT3:BOOL=TRUE',
            '-DOPT4:BOOL=TRUE',
            '-DOPT5:BOOL=TRUE',
            '-DOPT6:BOOL=TRUE',
            '-DOPT7:BOOL=TRUE',
            '-DOPT8:BOOL=FALSE',
            '-DOPT9:BOOL=FALSE',
            '-DOPT10:BOOL=FALSE',
            '-DOPT11:BOOL=FALSE',
            '-DOPT12:BOOL=FALSE',
            '-DOPT13:BOOL=FALSE',
            '-DOPT14=12',
            '-DOPT15=',
            '-DOPT16=',
            '-DOPT17:PATH=foo'])

    def test_operations(self):

        options1 = CMakeOptions()
        options1.define("OPT1_1", 'VAL1')
        options1.define("OPT1_2", 'VAL2')

        options2 = CMakeOptions()
        options2.define("OPT2_1", 'VAL3')

        options = options1 + options2
        self.assertIsInstance(options, CMakeOptions)
        self.assertEqual(list(options), [
            "-DOPT1_1=VAL1",
            "-DOPT1_2=VAL2",
            "-DOPT2_1=VAL3"])

        options_added = options + ["-CUSTOM", "12"]
        self.assertIsInstance(options_added, CMakeOptions)
        self.assertEqual(list(options_added), [
            "-DOPT1_1=VAL1",
            "-DOPT1_2=VAL2",
            "-DOPT2_1=VAL3",
            "-CUSTOM", "12"])

        options += options2
        self.assertIsInstance(options, CMakeOptions)
        self.assertEqual(list(options), [
            "-DOPT1_1=VAL1",
            "-DOPT1_2=VAL2",
            "-DOPT2_1=VAL3",
            "-DOPT2_1=VAL3"])

        options += ["-G", "Ninja"]
        self.assertIsInstance(options, CMakeOptions)
        self.assertEqual(list(options), [
            "-DOPT1_1=VAL1",
            "-DOPT1_2=VAL2",
            "-DOPT2_1=VAL3",
            "-DOPT2_1=VAL3",
            "-G", "Ninja"])

        list_options = ["-G", "Ninja"]
        list_options += options1
        self.assertIsInstance(list_options, list)
        self.assertEqual(list_options, [
            "-G", "Ninja",
            "-DOPT1_1=VAL1",
            "-DOPT1_2=VAL2"])


if __name__ == '__main__':
    unittest.main()
