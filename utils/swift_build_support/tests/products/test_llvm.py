# tests/products/test_llvm.py -----------------------------------*- python -*-
#
# This source file is part of the LLVM.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the LLVM project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of LLVM project authors
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
from swift_build_support.products import LLVM
from swift_build_support.toolchain import host_toolchain
from swift_build_support.workspace import Workspace


class LLVMTestCase(unittest.TestCase):

    def setUp(self):
        # Setup workspace
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, 'llvm'))

        self.workspace = Workspace(source_root=tmpdir1,
                                   build_root=tmpdir2)

        # Setup toolchain
        self.toolchain = host_toolchain()
        self.toolchain.cc = '/path/to/cc'
        self.toolchain.cxx = '/path/to/cxx'

        # Setup args
        self.args = argparse.Namespace(
            llvm_targets_to_build='X86;ARM;AArch64;PowerPC;SystemZ',
            llvm_assertions='true',
            darwin_deployment_version_osx='10.9')

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

    def test_llvm_targets_to_build(self):
        llvm = LLVM(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        expected_targets = 'X86;ARM;AArch64;PowerPC;SystemZ'
        expected_arg = '-DLLVM_TARGETS_TO_BUILD=%s' % expected_targets
        self.assertIn(expected_arg, llvm.cmake_options)

    def test_llvm_enable_assertions(self):
        self.args.llvm_assertions = True
        llvm = LLVM(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertIn('-DLLVM_ENABLE_ASSERTIONS=TRUE', llvm.cmake_options)

        self.args.llvm_assertions = False
        llvm = LLVM(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        self.assertIn('-DLLVM_ENABLE_ASSERTIONS=FALSE', llvm.cmake_options)

