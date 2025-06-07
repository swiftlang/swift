# tests/products/test_ninja.py ----------------------------------*- python -*-
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
import platform
import shutil
import sys
import tempfile
import unittest
from io import StringIO

from swift_build_support import shell
from swift_build_support.products import Ninja
from swift_build_support.targets import StdlibDeploymentTarget
from swift_build_support.toolchain import host_toolchain
from swift_build_support.workspace import Workspace


class NinjaTestCase(unittest.TestCase):

    def setUp(self):
        # Setup workspace
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, 'ninja'))

        self.workspace = Workspace(source_root=tmpdir1,
                                   build_root=tmpdir2)

        self.host = StdlibDeploymentTarget.host_target()

        # Setup toolchain
        self.toolchain = host_toolchain()
        self.toolchain.cc = '/path/to/cc'
        self.toolchain.cxx = '/path/to/cxx'

        # Setup args
        self.args = argparse.Namespace(
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

    def test_ninja_bin_path(self):
        ninja_build = Ninja.new_builder(
            args=self.args,
            toolchain=self.toolchain,
            workspace=self.workspace)

        self.assertEqual(ninja_build.ninja_bin_path,
                         os.path.join(
                             self.workspace.build_dir('build', 'ninja'),
                             'ninja'))

    def test_build(self):
        ninja_build = Ninja.new_builder(
            args=self.args,
            toolchain=self.toolchain,
            workspace=self.workspace)

        ninja_build.build()

        self.assertEqual(self.stdout.getvalue(), f"""\
--- Local Ninja Build ---
+ {self.toolchain.cmake} \
-S {self.workspace.source_dir('ninja')} \
-B {self.workspace.build_dir('build', 'ninja')} \
-DCMAKE_BUILD_TYPE=Release \
-DBUILD_TESTING=OFF \
-DCMAKE_C_COMPILER=/path/to/cc \
-DCMAKE_CXX_COMPILER=/path/to/cxx
+ {self.toolchain.cmake} --build {self.workspace.build_dir('build', 'ninja')}
""")

    def _platform_quote(self, path):
        if platform.system() == 'Windows':
            return "'{}'".format(path)
        else:
            return path
