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
try:
    # py2
    from StringIO import StringIO
except ImportError:
    # py3
    from io import StringIO

# from swift_build_support import cmake
from swift_build_support import shell
# from swift_build_support.products import CMark
from swift_build_support.targets import StdlibDeploymentTarget
from swift_build_support.toolchain import host_toolchain
from swift_build_support.workspace import Workspace


class CMarkTestCase(unittest.TestCase):

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

        # Setup args
        self.args = argparse.Namespace(
            build_cmark=True,
            cmake_generator="Ninja",
            cmark_build_type="Release",
            rebuild=False,
            extra_cmake_options=[],
            skip_build=False,
            darwin_deployment_version_osx="10.9",
            cmark_build_variant="Debug",
            export_compile_commands=False,
            reconfigure=False,
            distcc=None,
            sccache=None,
            cmake_c_launcher=None,
            cmake_cxx_launcher=None,
            clang_user_visible_version=None,
            build_ninja=False,
            enable_asan=False,
            enable_lsan=False,
            enable_sanitize_coverage=False,
            enable_tsan=False,
            enable_ubsan=False)

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

    def test_build(self):
        # Test disabled until we've moved to cmake toolchains
        True
#         cmark = CMark(
#             args=self.args,
#             toolchain=self.toolchain,
#             source_dir=self.workspace.source_root,
#             build_dir=self.workspace.build_root)

#         cmark.build(host_target=self.host.name)
#         _cmake = cmake.CMake(self.args, self.toolchain)

#         self.assertEqual(self.stdout.getvalue(), """\
# + pushd {build_dir}
# + {cmake} -DCMAKE_BUILD_TYPE:STRING={build_variant} {cmake_args} {source_dir}
# + popd
# + {cmake} --build {build_dir} --config {build_variant} -- all
# """.format(build_dir=self.workspace.build_root,
#            source_dir=self.workspace.source_root,
#            cmake=self.toolchain.cmake,
#            cmake_args=' '.join(_cmake.common_options()),
#            build_variant=self.args.cmark_build_variant))
