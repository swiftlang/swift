# tests/products/test_tsan_libdispatch.py ------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
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
from swift_build_support.products import TSanLibDispatch
from swift_build_support.targets import StdlibDeploymentTarget
from swift_build_support.workspace import Workspace


class BuildScriptHelperBuilderTestCase(unittest.TestCase):
    def setUp(self):
        # Setup workspace
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, 'compiler-rt'))

        self.host = StdlibDeploymentTarget.host_target()

        self.workspace = Workspace(source_root=tmpdir1,
                                   build_root=tmpdir2)

        # Setup args
        self.args = argparse.Namespace(
            install_destdir='/dest/dir/path')

        # Setup shell
        shell.dry_run = True
        self._orig_stdout = sys.stdout
        self._orig_stderr = sys.stderr
        self.stdout = StringIO()
        self.stderr = StringIO()
        sys.stdout = self.stdout
        sys.stderr = self.stderr

        # Setup some expected values which are reused in many tests
        # self.expected_source_dir = self.workspace.source_dir(
        #     MockProduct.product_source_name())
        # self.expected_script_path = os.path.join(self.expected_source_dir,
        #                                          'Utilities',
        #                                          'build-script-helper.py')
        # self.expected_build_dir = self.workspace.build_dir(
        #     self.host.name, MockProduct.product_name())
        # self.expected_toolchain_path = self.args.install_destdir
        # if platform.system() == 'Darwin':
        #     self.expected_toolchain_path += '/install/prefix/path.toolchain'

    def tearDown(self):
        shutil.rmtree(self.workspace.build_root)
        shutil.rmtree(self.workspace.source_root)
        sys.stdout = self._orig_stdout
        sys.stderr = self._orig_stderr
        shell.dry_run = False
        self.workspace = None
        self.args = None

    def test_build(self):
        builder = TSanLibDispatch.new_builder(
            self.args, None, self.workspace, self.host)
        builder.build()

        build_dir = self.workspace.build_dir(
            self.host.name, TSanLibDispatch.product_name())

        cmake_cmd = "cmake -GNinja "\
                    "-DCMAKE_PREFIX_PATH=/dest/dir/path/usr "\
                    "-DCMAKE_C_COMPILER=/dest/dir/path/usr/bin/clang "\
                    "-DCMAKE_CXX_COMPILER=/dest/dir/path/usr/bin/clang++ "\
                    "-DCMAKE_BUILD_TYPE=Release "\
                    "-DLLVM_ENABLE_ASSERTIONS=ON "\
                    "-DCOMPILER_RT_INCLUDE_TESTS=ON "\
                    "-DCOMPILER_RT_BUILD_XRAY=OFF "\
                    "-DCOMPILER_RT_INTERCEPT_LIBDISPATCH=ON "\
                    "-DCOMPILER_RT_LIBDISPATCH_INSTALL_PATH="\
                    "/dest/dir/path/usr " +\
                    self.workspace.source_dir('compiler-rt')

        expected_output = """\
+ rm -rf {}
+ mkdir -p {}
+ pushd {}
+ {}
+ ninja tsan
+ popd
""".format(build_dir, build_dir, build_dir, cmake_cmd)
        self.assertEqual(self.stdout.getvalue(), expected_output)

    def test_test(self):
        builder = TSanLibDispatch.new_builder(
            self.args, None, self.workspace, self.host)
        builder.test()

        expected_output = """\
+ pushd {}
+ env LIT_FILTER=libdispatch ninja check-tsan
+ popd
""".format(self.workspace.build_dir(self.host.name,
                                    TSanLibDispatch.product_name()))
        self.assertEqual(self.stdout.getvalue(), expected_output)
