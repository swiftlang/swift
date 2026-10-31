# tests/products/test_swifttesting.py ---------------------------*- python -*-
#
# This source file is part of the LLVM.org open source project
#
# Copyright (c) 2014 - 2026 Apple Inc. and the LLVM project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of LLVM project authors
# ----------------------------------------------------------------------------

import argparse
import os
import shutil
import sys
import tempfile
import unittest
from io import StringIO

from swift_build_support import shell
from swift_build_support.products import SwiftTesting
from swift_build_support.toolchain import host_toolchain
from swift_build_support.workspace import Workspace


class SwiftTestingTestCase(unittest.TestCase):

    def setUp(self):
        # Setup workspace
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, 'swift-testing'))

        self.workspace = Workspace(source_root=tmpdir1,
                                   build_root=tmpdir2)

        # Setup toolchain
        self.toolchain = host_toolchain()
        self.toolchain.cc = '/path/to/cc'
        self.toolchain.cxx = '/path/to/cxx'

        # Setup args
        self.args = argparse.Namespace(
            )

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

    def test_targets_linux(self):
        swifttesting_product = SwiftTesting(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        targets = []
        swifttesting_product._for_each_swifttesting_target("linux-x86_64",
            lambda target: targets.append(target))
        self.assertEqual(["linux-x86_64"], targets)

    def test_targets_macos_when_one_specified(self):
        self.args.stdlib_deployment_targets = [ "macosx-x86_64", "macosx-arm64", "iphoneos-arm64" ]
        self.args.build_stdlib_deployment_targets = [ "macosx-arm64" ]
        self.args.host_target = "macosx-arm64"
        self.args.build_osx = True
        self.args.build_ios_device = True
        swifttesting_product = SwiftTesting(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        targets = []
        swifttesting_product._for_each_swifttesting_target("macosx-arm64",
            lambda target: targets.append(target))
        self.assertEqual(sorted(["macosx-arm64"]), sorted(targets))

    def test_targets_macos(self):
        self.args.stdlib_deployment_targets = [ "macosx-x86_64", "macosx-arm64", "iphoneos-arm64" ]
        self.args.build_stdlib_deployment_targets = "all"
        self.args.host_target = "macosx-arm64"
        self.args.build_osx = True
        self.args.build_ios_device = True
        swifttesting_product = SwiftTesting(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        targets = []
        swifttesting_product._for_each_swifttesting_target("macosx-arm64",
            lambda target: targets.append(target))
        self.assertEqual(sorted(["macosx-x86_64", "macosx-arm64"]), sorted(targets))
