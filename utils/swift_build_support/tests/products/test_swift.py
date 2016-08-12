# tests/products/test_swift.py ----------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
            darwin_deployment_version_osx="10.9")

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

    def test_swift_runtime_tsan(self):
        self.args.enable_tsan_runtime = True
        swift = Swift(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertEqual(swift.cmake_options,
                         ['-DSWIFT_RUNTIME_USE_SANITIZERS=Thread'])

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
