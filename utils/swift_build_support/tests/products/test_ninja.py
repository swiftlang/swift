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
try:
    # py2
    from StringIO import StringIO
except ImportError:
    # py3
    from io import StringIO

from swift_build_support import shell
from swift_build_support import xcrun
from swift_build_support.products import Ninja
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

        # Setup toolchain
        self.toolchain = host_toolchain()
        self.toolchain.cc = '/path/to/cc'
        self.toolchain.cxx = '/path/to/cxx'

        # Setup args
        self.args = argparse.Namespace(
            build_ninja=True,
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
        ninja_build = Ninja(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')

        self.assertEqual(ninja_build.ninja_bin_path, '/path/to/build/ninja')

    def test_do_build(self):
        ninja_build = Ninja(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=self.workspace.source_dir('ninja'),
            build_dir=self.workspace.build_dir('build', 'ninja'))

        ninja_build.do_build()

        expect_env = ""
        if platform.system() == "Darwin":
            expect_env = (
                "env "
                "'CFLAGS=-isysroot {sysroot} -mmacosx-version-min=10.9' "
                "CXX={cxx} "
                "LDFLAGS=-mmacosx-version-min=10.9 "
            ).format(
                cxx=self.toolchain.cxx,
                sysroot=xcrun.sdk_path('macosx')
            )
        elif self.toolchain.cxx:
            expect_env = (
                "env "
                "CXX={cxx} "
            ).format(
                cxx=self.toolchain.cxx,
            )

        self.assertEqual(self.stdout.getvalue(), """\
+ rm -rf {build_dir}
+ cp -r {source_dir} {build_dir}
+ pushd {build_dir}
+ {expect_env}{python} configure.py --bootstrap
+ popd
""".format(
            source_dir=os.path.join(self.workspace.source_root, 'ninja'),
            build_dir=os.path.join(self.workspace.build_root, 'ninja-build'),
            expect_env=expect_env,
            python=sys.executable))
