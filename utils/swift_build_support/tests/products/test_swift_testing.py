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
from swift_build_support.targets import StdlibDeploymentTarget


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
            cross_compile_hosts = [],
            swift_darwin_supported_archs = None)

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

    def test_targets_linux_only_host(self):
        swifttesting_product = SwiftTesting(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        targets = []
        swifttesting_product._for_each_host_and_macos_target("linux-x86_64",
            lambda target: targets.append(target))
        self.assertEqual(["linux-x86_64"], targets)

    def test_targets_linux_with_cross_compile_hosts(self):
        self.args.cross_compile_hosts.append("linux-arm64")
        swifttesting_product = SwiftTesting(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')

        for host in ["linux-x86_64", "linux-arm64"]:
            targets = []
            swifttesting_product._for_each_host_and_macos_target(host,
                lambda target: targets.append(target))
            self.assertEqual([host], targets)

    def test_targets_macos_only_host(self):
        self.args.host_target = "macosx-arm64"
        swifttesting_product = SwiftTesting(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')
        targets = []
        swifttesting_product._for_each_host_and_macos_target("macosx-arm64",
            lambda target: targets.append(target))
        self.assertEqual(sorted([target.name for target in StdlibDeploymentTarget.OSX.targets]), sorted(targets))

    def test_targets_macos_with_cross_compile_hosts_swift_darwin_supported_archs(self):
        self.args.host_target = "macosx-arm64"
        self.args.swift_darwin_supported_archs = [ "arm64" ]
        self.args.cross_compile_hosts.append("macosx-x86_64")
        swifttesting_product = SwiftTesting(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')

        for host in ["macosx-arm64", "macosx-x86_64"]:
            targets = []
            swifttesting_product._for_each_host_and_macos_target(host,
                lambda target: targets.append(target))
            self.assertEqual([host], targets)
    
    def test_targets_macos_with_cross_compile_hosts_different_from_macos_targets(self):
        self.args.host_target = "macosx-arm64"
        self.args.cross_compile_hosts.append("android-armv7")
        swifttesting_product = SwiftTesting(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')

        targets = []
        swifttesting_product._for_each_host_and_macos_target("macosx-arm64",
            lambda target: targets.append(target))
        self.assertEqual(sorted([target.name for target in StdlibDeploymentTarget.OSX.targets]), sorted(targets))

        targets = []
        swifttesting_product._for_each_host_and_macos_target("android-armv7",
            lambda target: targets.append(target))
        self.assertEqual(["android-armv7"], targets)
