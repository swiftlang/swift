# tests/products/test_swift.py ----------------------------------*- python -*-
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
from swift_build_support.products import LLDB
from swift_build_support.toolchain import host_toolchain
from swift_build_support.workspace import Workspace


class LLDBTestCase(unittest.TestCase):

    def setUp(self):
        # Setup workspace
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, 'swift'))

        self.workspace = Workspace(source_root=tmpdir1,
                                   build_root=tmpdir2)

        # Setup toolchain
        self.toolchain = host_toolchain()
        self.toolchain.cc = '/path/to/cc'
        self.toolchain.cxx = '/path/to/cxx'
        self.toolchain.python3 = '/path/to/python3'

        # Setup args
        self.args = argparse.Namespace(
            extra_lldb_cmake_options='',
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

    @classmethod
    def isDarwin(cls):
        return platform.system() == 'Darwin'

    def test_default_options(self):
        lldb = LLDB(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')

        if LLDBTestCase.isDarwin(): 
            expected = [
              '-DPython3_EXECUTABLE=/path/to/python3',
            ]
        else:
            expected = []

        self.assertEqual(set(lldb.cmake_options), set(expected))

    def test_extra_lldb_cmake_options(self):
        self.args.extra_lldb_cmake_options = [
            "--trace-expand", "-DPython3_EXECUTABLE=/different/path/to/python3"
        ]

        lldb = LLDB(
            args=self.args,
            toolchain=self.toolchain,
            source_dir='/path/to/src',
            build_dir='/path/to/build')

        expected = [ 
            "--trace-expand", "-DPython3_EXECUTABLE=/different/path/to/python3"
        ]
        if LLDBTestCase.isDarwin(): 
            expected.append(
                '-DPython3_EXECUTABLE=/path/to/python3',
            )

        self.assertEqual(set(lldb.cmake_options), set(expected))
        # Ensure we are indeed overriding the path to the Python interpreter
        self.assertEqual("-DPython3_EXECUTABLE=/different/path/to/python3", lldb.cmake_options._options[-1])
