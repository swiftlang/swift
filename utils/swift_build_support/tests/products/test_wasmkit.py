# tests/products/test_wasmkit.py --------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
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
from io import StringIO

from swift_build_support import shell
from swift_build_support.products import WasmKit
from swift_build_support.toolchain import host_toolchain
from swift_build_support.workspace import Workspace


class WasmKitTestCase(unittest.TestCase):

    def setUp(self):
        # Setup workspace
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, 'wasmkit'))

        self.workspace = Workspace(source_root=tmpdir1,
                                   build_root=tmpdir2)

        # Setup toolchain
        self.toolchain = host_toolchain()
        self.toolchain.cc = '/path/to/cc'
        self.toolchain.cxx = '/path/to/cxx'

        # Setup args
        self.args = argparse.Namespace(
            verbose_build = True,
            build_jobs = 1,
            cross_compile_hosts = "",
            install_prefix = "install_prefix",
            install_destdir="/path/to/install_destdir")

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

    def test_no_arch_flag_linux(self):
        wasmkit_product = WasmKit(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        wasmkit_product.build("linux-x86_64")
        self.assertNotIn("-arch ", self.stdout)

    def test_one_arch_flag_macos(self):
        wasmkit_product = WasmKit(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        wasmkit_product.build("macosx-arm64")
        self.assertIn("-arch arm64", self.stdout.getvalue())

    def test_two_arch_flags_crosscompiling_macos(self):
        self.args.cross_compile_hosts = [ "macosx-x86_64", "iphonesimulator-armv7s"]
        self.cross_compile_append_host_target_to_destdir = True
        wasmkit_product = WasmKit(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        wasmkit_product.build("macosx-arm64")
        command_output = self.stdout.getvalue()
        self.assertIn("-arch arm64", command_output)
        self.assertIn("-arch x86_64", command_output)
