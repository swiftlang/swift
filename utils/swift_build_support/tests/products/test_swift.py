# tests/products/test_swift.py ----------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
# ----------------------------------------------------------------------------

import argparse
import os
import shutil
import sys
import tempfile
import unittest
try:
    # py2
    from StringIO import StringIO
except ImportError:
    # py3
    from io import StringIO

from swift_build_support import shell
from swift_build_support.products import Swift
from swift_build_support.toolchain import host_toolchain
from swift_build_support.workspace import Workspace


class SwiftTestCase(unittest.TestCase):

    def setUp(self):
        # Setup workspace
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, 'swift'))

        self.workspace = Workspace(source_root=tmpdir1,
                                   build_root=tmpdir2)

        # Setup toolchain
        self.toolchain = host_toolchain()
        self.toolchain.cc = '/path/to/cc'
        self.toolchain.cxx = '/path/to/cxx'

        # Setup args
        self.args = argparse.Namespace(
            enable_tsan_runtime=False,
            compiler_vendor='none',
            swift_compiler_version=None,
            clang_compiler_version=None,
            swift_user_visible_version=None,
            darwin_deployment_version_osx="10.9",
            benchmark=False,
            benchmark_num_onone_iterations=3,
            benchmark_num_o_iterations=3,
            enable_sil_ownership=False,
            force_optimized_typechecker=False)

        # Setup shell
        shell.dry_run = True
        self._orig_stdout = sys.stdout
        self._orig_stderr = sys.stderr
        self.stdout = StringIO()
        self.stderr = StringIO()
        sys.stdout = self.stdout
        sys.stderr = self.stderr

    def tearDown(self):
        shutil.rmtree(self.workspace.build_root)
        shutil.rmtree(self.workspace.source_root)
        sys.stdout = self._orig_stdout
        sys.stderr = self._orig_stderr
        shell.dry_run = False
        self.workspace = None
        self.toolchain = None
        self.args = None

    def test_by_default_no_cmake_options(self):
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertEqual(set(swift.cmake_options), set([
                         '-DCMAKE_EXPORT_COMPILE_COMMANDS=TRUE',
                         '-DSWIFT_STDLIB_ENABLE_SIL_OWNERSHIP=FALSE',
                         '-DSWIFT_FORCE_OPTIMIZED_TYPECHECKER=FALSE']))

    def test_swift_runtime_tsan(self):
        self.args.enable_tsan_runtime = True
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertEqual(set(swift.cmake_options),
                         set(['-DSWIFT_RUNTIME_USE_SANITIZERS=Thread',
                              '-DCMAKE_EXPORT_COMPILE_COMMANDS=TRUE',
                              '-DSWIFT_STDLIB_ENABLE_SIL_OWNERSHIP=FALSE',
                              '-DSWIFT_FORCE_OPTIMIZED_TYPECHECKER=FALSE']))

    def test_swift_compiler_vendor_flags(self):
        self.args.compiler_vendor = "none"
        self.args.swift_user_visible_version = None
        self.args.swift_compiler_version = None
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertListEqual(
            [], [x for x in swift.cmake_options if 'SWIFT_VENDOR' in x])
        self.assertListEqual(
            [], [x for x in swift.cmake_options if 'SWIFT_VENDOR_UTI' in x])
        self.assertListEqual(
            [], [x for x in swift.cmake_options if 'SWIFT_VERSION' in x])
        self.assertListEqual(
            [],
            [x for x in swift.cmake_options if 'SWIFT_COMPILER_VERSION' in x])

        self.args.compiler_vendor = "apple"
        self.args.swift_user_visible_version = "1.3"
        self.args.swift_compiler_version = None
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertIn('-DSWIFT_VENDOR=Apple', swift.cmake_options)
        self.assertIn(
            '-DSWIFT_VENDOR_UTI=com.apple.compilers.llvm.swift',
            swift.cmake_options)
        self.assertIn('-DSWIFT_VERSION=1.3', swift.cmake_options)
        self.assertIn('-DSWIFT_COMPILER_VERSION=', swift.cmake_options)

        self.args.compiler_vendor = "apple"
        self.args.swift_user_visible_version = "1.3"
        self.args.swift_compiler_version = "2.3"
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertIn('-DSWIFT_VENDOR=Apple', swift.cmake_options)
        self.assertIn(
            '-DSWIFT_VENDOR_UTI=com.apple.compilers.llvm.swift',
            swift.cmake_options)
        self.assertIn('-DSWIFT_VERSION=1.3', swift.cmake_options)
        self.assertIn('-DSWIFT_COMPILER_VERSION=2.3', swift.cmake_options)

        self.args.compiler_vendor = "unknown"
        with self.assertRaises(RuntimeError):
            swift = Swift(
                args=self.args,
                toolchain=self.toolchain,
                source_dir='/path/to/src',
                build_dir='/path/to/build')

    def test_version_flags(self):
        # First make sure that by default, we do not get any version flags.
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertListEqual(
            [],
            [x for x in swift.cmake_options if 'SWIFT_COMPILER_VERSION' in x]
        )
        self.assertListEqual(
            [],
            [x for x in swift.cmake_options if 'CLANG_COMPILER_VERSION' in x]
        )

        self.args.swift_compiler_version = "3.0"
        self.args.clang_compiler_version = None
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertListEqual(
            ['-DSWIFT_COMPILER_VERSION=3.0'],
            [x for x in swift.cmake_options if 'SWIFT_COMPILER_VERSION' in x]
        )
        self.assertListEqual(
            [],
            [x for x in swift.cmake_options if 'CLANG_COMPILER_VERSION' in x]
        )

        self.args.swift_compiler_version = None
        self.args.clang_compiler_version = "3.8.0"
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertListEqual(
            [],
            [x for x in swift.cmake_options if 'SWIFT_COMPILER_VERSION' in x]
        )
        self.assertListEqual(
            ['-DCLANG_COMPILER_VERSION=3.8.0'],
            [x for x in swift.cmake_options if 'CLANG_COMPILER_VERSION' in x]
        )

        self.args.swift_compiler_version = "1.0"
        self.args.clang_compiler_version = "1.9.3"
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertListEqual(
            ['-DSWIFT_COMPILER_VERSION=1.0'],
            [x for x in swift.cmake_options if 'SWIFT_COMPILER_VERSION' in x]
        )
        self.assertListEqual(
            ['-DCLANG_COMPILER_VERSION=1.9.3'],
            [x for x in swift.cmake_options if 'CLANG_COMPILER_VERSION' in x]
        )
        self.args.swift_compiler_version = None
        self.args.clang_compiler_version = None

    def test_benchmark_flags(self):
        self.args.benchmark = True
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        # By default, we should get an argument of 3 for both -Onone and -O
        self.assertEqual(
            ['-DSWIFT_BENCHMARK_NUM_ONONE_ITERATIONS=3',
             '-DSWIFT_BENCHMARK_NUM_O_ITERATIONS=3'],
            [x for x in swift.cmake_options if 'SWIFT_BENCHMARK_NUM' in x])

        self.args.benchmark_num_onone_iterations = 20
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertEqual(
            ['-DSWIFT_BENCHMARK_NUM_ONONE_ITERATIONS=20',
             '-DSWIFT_BENCHMARK_NUM_O_ITERATIONS=3'],
            [x for x in swift.cmake_options if 'SWIFT_BENCHMARK_NUM' in x])
        self.args.benchmark_num_onone_iterations = 3

        self.args.benchmark_num_o_iterations = 30
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertEqual(
            ['-DSWIFT_BENCHMARK_NUM_ONONE_ITERATIONS=3',
             '-DSWIFT_BENCHMARK_NUM_O_ITERATIONS=30'],
            [x for x in swift.cmake_options if 'SWIFT_BENCHMARK_NUM' in x])
        self.args.benchmark_num_onone_iterations = 3

        self.args.benchmark_num_onone_iterations = 10
        self.args.benchmark_num_o_iterations = 25
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertEqual(
            ['-DSWIFT_BENCHMARK_NUM_ONONE_ITERATIONS=10',
             '-DSWIFT_BENCHMARK_NUM_O_ITERATIONS=25'],
            [x for x in swift.cmake_options if 'SWIFT_BENCHMARK_NUM' in x])

    def test_sil_ownership_flags(self):
        self.args.enable_sil_ownership = True
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertEqual(
            ['-DSWIFT_STDLIB_ENABLE_SIL_OWNERSHIP=TRUE'],
            [x for x in swift.cmake_options
             if 'SWIFT_STDLIB_ENABLE_SIL_OWNERSHIP' in x])

    def test_force_optimized_typechecker_flags(self):
        self.args.force_optimized_typechecker = True
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertEqual(
            ['-DSWIFT_FORCE_OPTIMIZED_TYPECHECKER=TRUE'],
            [x for x in swift.cmake_options
             if 'SWIFT_FORCE_OPTIMIZED_TYPECHECKER' in x])
