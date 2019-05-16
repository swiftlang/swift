# tests/products/test_benchmarks.py -----------------------------*- python -*-
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
from swift_build_support.products.benchmarks import Benchmarks
from swift_build_support.targets import StdlibDeploymentTarget
from swift_build_support.workspace import Workspace


class BenchmarkTestCase(unittest.TestCase):
    def setUp(self):
        # Setup workspace
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, 'swift', 'benchmarks'))

        self.workspace = Workspace(source_root=tmpdir1,
                                   build_root=tmpdir2)

        self.host = StdlibDeploymentTarget.host_target()

        # Setup args
        self.args = argparse.Namespace(
            install_destdir='/dest/dir/path',
            install_prefix='/install/prefix/path.toolchain/usr')

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
        self.args = None

    def test_build(self):
        builder = Benchmarks.new_builder(
            self.args, None, self.workspace, self.host)
        builder.build()

        expected_toolchain_path = self.args.install_destdir
        if platform.system() == 'Darwin':
            expected_toolchain_path += '/install/prefix/path.toolchain'

        expected_output = "+ {} --verbose --package-path {} --build-path {} "\
                          "--toolchain {}\n".format(
                              os.path.join(self.workspace.source_dir('swift'),
                                           'benchmark', 'scripts',
                                           'build_script_helper.py'),
                              os.path.join(self.workspace.source_dir('swift'),
                                           'benchmark'),
                              self.workspace.build_dir(self.host.name,
                                                       'benchmarks'),
                              expected_toolchain_path)
        self.assertEqual(self.stdout.getvalue(), expected_output)

    def test_test(self):
        builder = Benchmarks.new_builder(
            self.args, None, self.workspace, self.host)
        builder.test()

        expected_output = """\
+ {bin_dir}/Benchmark_Onone --num-iters=1 XorLoop
+ {bin_dir}/Benchmark_O --num-iters=1 XorLoop
+ {bin_dir}/Benchmark_Osize --num-iters=1 XorLoop
""".format(bin_dir=os.path.join(
            self.workspace.build_dir(
                self.host.name, 'benchmarks'), 'bin'))
        self.assertEqual(self.stdout.getvalue(), expected_output)
