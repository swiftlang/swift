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
import platform
import unittest
from argparse import Namespace

from build_swift.build_swift.versions import Version

from swift_build_support.cmake import CMake, CMakeOptions
from swift_build_support.toolchain import host_toolchain


class CMakeTestCase(unittest.TestCase):

    def mock_distcc_path(self):
        """Return a path string of mock distcc executable
        """
        if platform.system() == 'Windows':
            executable = 'mock-distcc.cmd'
        else:
            executable = 'mock-distcc'
        return os.path.join(os.path.dirname(__file__), executable)

    def mock_sccache_path(self):
        """Return a path string of a mock sccache executable
        """
        if platform.system() == 'Windows':
            executable = 'sccache.cmd'
        else:
            executable = 'sccache'
        return os.path.join(os.path.dirname(__file__), executable)

    def default_args(self):
        """Return new args object with default values
        """
        return Namespace(host_cc="/path/to/clang",
                         host_cxx="/path/to/clang++",
                         host_swiftc="/path/to/swiftc",
                         host_libtool="/path/to/libtool",
                         host_ar="/path/to/ar",
                         host_ranlib="/path/to/ranlib",
                         enable_asan=False,
                         enable_ubsan=False,
                         enable_tsan=False,
                         enable_lsan=False,
                         enable_sanitize_coverage=False,
                         export_compile_commands=False,
                         distcc=False,
                         sccache=False,
                         cmake_generator="Ninja",
                         cmake_c_launcher=None,
                         cmake_cxx_launcher=None,
                         clang_compiler_version=None,
                         clang_user_visible_version=None,
                         build_jobs=8,
                         build_args=[],
                         verbose_build=False,
                         build_ninja=False)

    def which_ninja(self, args):
        # Maybe we'll build a ninja, maybe we wont.
        # Fake it anyway for the tests.
        return '/path/to/built/ninja'

    def cmake(self, args):
        """Return new CMake object initialized with given args
        """
        toolchain = host_toolchain()
        toolchain.cc = args.host_cc
        toolchain.cxx = args.host_cxx
        toolchain.swiftc = args.host_swiftc
        toolchain.libtool = args.host_libtool
        toolchain.ar = args.host_ar
        toolchain.ranlib = args.host_ranlib
        if args.distcc:
            toolchain.distcc = self.mock_distcc_path()
        if args.sccache:
            toolchain.sccache = self.mock_sccache_path()
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
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
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
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
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
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
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
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
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
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
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
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
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
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_lsan(self):
        args = self.default_args()
        args.enable_lsan = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DLLVM_USE_SANITIZER=Leaks",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_coverage_sanitizer(self):
        args = self.default_args()
        args.enable_sanitize_coverage = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DLLVM_USE_SANITIZE_COVERAGE=ON",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
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
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_distcc(self):
        args = self.default_args()
        args.distcc = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DCMAKE_C_COMPILER_LAUNCHER:PATH=" + self.mock_distcc_path(),
             "-DCMAKE_CXX_COMPILER_LAUNCHER:PATH=" + self.mock_distcc_path(),
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_sccache(self):
        args = self.default_args()
        args.sccache = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DCMAKE_C_COMPILER_LAUNCHER:PATH=" + self.mock_sccache_path(),
             "-DCMAKE_CXX_COMPILER_LAUNCHER:PATH=" + self.mock_sccache_path(),
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_launcher(self):
        args = self.default_args()
        cmake_c_launcher = "/path/to/c_launcher"
        cmake_cxx_launcher = "/path/to/cxx_launcher"
        args.cmake_c_launcher = cmake_c_launcher
        args.cmake_cxx_launcher = cmake_cxx_launcher
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DCMAKE_C_COMPILER_LAUNCHER:PATH=" + cmake_c_launcher,
             "-DCMAKE_CXX_COMPILER_LAUNCHER:PATH=" + cmake_cxx_launcher,
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_clang_compiler_version(self):
        args = self.default_args()
        args.clang_compiler_version = Version("999.0.999")
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_clang_user_visible_version(self):
        args = self.default_args()
        args.clang_user_visible_version = Version("9.0.0")
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
             "-DLLVM_VERSION_MAJOR:STRING=9",
             "-DLLVM_VERSION_MINOR:STRING=0",
             "-DLLVM_VERSION_PATCH:STRING=0",
             "-DCLANG_VERSION_MAJOR:STRING=9",
             "-DCLANG_VERSION_MINOR:STRING=0",
             "-DCLANG_VERSION_PATCH:STRING=0",
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
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

    def test_common_options_full(self):
        args = self.default_args()
        args.enable_asan = True
        args.enable_ubsan = True
        args.export_compile_commands = True
        args.distcc = True
        args.clang_user_visible_version = Version("9.0.0")
        args.clang_compiler_version = Version("999.0.900")
        args.build_ninja = True
        cmake = self.cmake(args)
        self.assertEqual(
            list(cmake.common_options()),
            ["-G", "Ninja",
             "-DLLVM_USE_SANITIZER=Address;Undefined",
             "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON",
             "-DCMAKE_C_COMPILER_LAUNCHER:PATH=" + self.mock_distcc_path(),
             "-DCMAKE_CXX_COMPILER_LAUNCHER:PATH=" + self.mock_distcc_path(),
             "-DCMAKE_C_COMPILER:PATH=/path/to/clang",
             "-DCMAKE_CXX_COMPILER:PATH=/path/to/clang++",
             "-DCMAKE_Swift_COMPILER:PATH=/path/to/swiftc",
             "-DCMAKE_LIBTOOL:PATH=/path/to/libtool",
             "-DCMAKE_AR:PATH=/path/to/ar",
             "-DCMAKE_RANLIB:PATH=/path/to/ranlib",
             "-DLLVM_VERSION_MAJOR:STRING=9",
             "-DLLVM_VERSION_MINOR:STRING=0",
             "-DLLVM_VERSION_PATCH:STRING=0",
             "-DCLANG_VERSION_MAJOR:STRING=9",
             "-DCLANG_VERSION_MINOR:STRING=0",
             "-DCLANG_VERSION_PATCH:STRING=0",
             "-DCMAKE_MAKE_PROGRAM=" + self.which_ninja(args)])

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

    def test_initial_options_with_tuples(self):
        options = CMakeOptions([('FOO', 'foo'), ('BAR', True)])
        self.assertIn('-DFOO=foo', options)
        self.assertIn('-DBAR=TRUE', options)

    def test_initial_options_with_other_options(self):
        options = CMakeOptions()
        options.define('FOO', 'foo')
        options.define('BAR', True)
        derived = CMakeOptions(options)
        self.assertIn('-DFOO=foo', derived)
        self.assertIn('-DBAR=TRUE', derived)

    def test_booleans_are_translated(self):
        options = CMakeOptions()
        options.define('A_BOOLEAN_OPTION', True)
        options.define('ANOTHER_BOOLEAN_OPTION', False)
        self.assertIn('-DA_BOOLEAN_OPTION=TRUE', options)
        self.assertIn('-DANOTHER_BOOLEAN_OPTION=FALSE', options)

    def test_extend_with_other_options(self):
        options = CMakeOptions()
        options.define('FOO', 'foo')
        options.define('BAR', True)
        derived = CMakeOptions()
        derived.extend(options)
        self.assertIn('-DFOO=foo', derived)
        self.assertIn('-DBAR=TRUE', derived)

    def test_extend_with_tuples(self):
        options = CMakeOptions()
        options.extend([('FOO', 'foo'), ('BAR', True)])
        self.assertIn('-DFOO=foo', options)
        self.assertIn('-DBAR=TRUE', options)

    def test_contains(self):
        options = CMakeOptions()
        self.assertTrue('-DFOO=foo' not in options)
        options.define('FOO', 'foo')
        self.assertTrue('-DFOO=foo' in options)


if __name__ == '__main__':
    unittest.main()
