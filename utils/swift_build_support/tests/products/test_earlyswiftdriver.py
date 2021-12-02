# tests/products/test_ninja.py ----------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2021 Apple Inc. and the Swift project authors
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
from swift_build_support.products import EarlySwiftDriver
from swift_build_support.targets import StdlibDeploymentTarget
from swift_build_support.toolchain import host_toolchain
from swift_build_support.workspace import Workspace


class EarlySwiftDriverTestCase(unittest.TestCase):

    def setUp(self):
        # Setup workspace
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, 'cmark'))

        self.workspace = Workspace(source_root=tmpdir1,
                                   build_root=tmpdir2)

        self.host = StdlibDeploymentTarget.host_target()

        # Setup toolchain
        self.toolchain = host_toolchain()
        self.toolchain.cc = '/path/to/cc'
        self.toolchain.cxx = '/path/to/cxx'

        self.cross_compile_hosts = ["macosx-arm64", "linux-x86_64", "linux-aarch64"]

        # Setup args
        self.args = argparse.Namespace(
            cross_compile_hosts=self.cross_compile_hosts)

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

    def test_should_build(self):
        early_swift_driver = EarlySwiftDriver(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=self.workspace.source_root,
            build_dir=self.workspace.build_root)

        for target in self.cross_compile_hosts:
            self.assertFalse(early_swift_driver.should_build(target))
