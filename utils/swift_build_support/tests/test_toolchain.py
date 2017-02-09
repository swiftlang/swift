# -*- python -*-
# test_toolchain.py - Unit tests for swift_build_support.toolchain
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import unittest

from swift_build_support import toolchain
from swift_build_support.toolchain import host_toolchain


def get_suffix(path, prefix):
    basename = os.path.basename(path)
    return basename[len(prefix):]


class ToolchainTestCase(unittest.TestCase):

    def test_clang_tools(self):
        tc = host_toolchain()

        self.assertIsNotNone(tc.cc)
        self.assertIsNotNone(tc.cxx)

        self.assertTrue(
            os.path.isabs(tc.cc) and
            os.path.basename(tc.cc).startswith('clang'))
        self.assertTrue(
            os.path.isabs(tc.cxx) and
            os.path.basename(tc.cxx).startswith('clang++'))

    def test_llvm_tools(self):
        tc = host_toolchain()

        self.assertTrue(
            tc.llvm_profdata is None or
            os.path.isabs(tc.llvm_profdata) and
            os.path.basename(tc.llvm_profdata).startswith('llvm-profdata'))
        self.assertTrue(
            tc.llvm_cov is None or
            os.path.isabs(tc.llvm_cov) and
            os.path.basename(tc.llvm_cov).startswith('llvm-cov'))

    def test_misc_tools(self):
        tc = host_toolchain()

        # CMake
        self.assertIsNotNone(tc.cmake)
        self.assertTrue(
            os.path.basename(tc.cmake).startswith('cmake'))

        # Ninja
        self.assertTrue(tc.ninja is None or
                        os.path.basename(tc.ninja) == 'ninja' or
                        os.path.basename(tc.ninja) == 'ninja-build')
        # distcc
        self.assertTrue(tc.distcc is None or
                        os.path.basename(tc.distcc) == 'distcc')
        # pump
        self.assertTrue(tc.distcc_pump is None or
                        os.path.basename(tc.distcc_pump) == 'pump' or
                        os.path.basename(tc.distcc_pump) == 'distcc-pump')

    def test_find_tool(self):
        tc = host_toolchain()

        # Toolchain.find_tool(path) can find arbitrary tool in PATH

        sh = tc.find_tool('sh')
        self.assertTrue(sh is not None and
                        os.path.isabs(sh) and
                        os.path.basename(sh) == 'sh')
        tar = tc.find_tool('tar')
        self.assertTrue(tar is not None and
                        os.path.isabs(tar) and
                        os.path.basename(tar) == 'tar')

    def test_tools_suffix_match(self):
        tc = host_toolchain()

        # CC and CXX must have consistent suffix
        cc_suffix = get_suffix(tc.cc, 'clang')
        cxx_suffix = get_suffix(tc.cxx, 'clang++')
        self.assertEqual(cc_suffix, cxx_suffix)

    def test_tools_llvm_suffix(self):
        tc = host_toolchain()

        cov_suffix = None
        profdata_suffix = None
        if tc.llvm_cov:
            cov_suffix = get_suffix(tc.llvm_cov, 'llvm-cov')
        if tc.llvm_profdata:
            profdata_suffix = get_suffix(tc.llvm_profdata, 'llvm-profdata')

        if profdata_suffix is not None and cov_suffix is not None:
            self.assertEqual(profdata_suffix, cov_suffix)

        # If we have suffixed clang, llvm tools must have the same suffix.
        cc_suffix = get_suffix(tc.cc, 'clang')
        if cc_suffix != '':
            if cov_suffix is not None:
                self.assertEqual(cc_suffix, cov_suffix)
            if profdata_suffix is not None:
                self.assertEqual(cc_suffix, profdata_suffix)

    def test_toolchain_instances(self):
        # Check that we can instantiate every toolchain, even if it isn't the
        # current platform.
        toolchain.MacOSX()
        toolchain.Linux()
        toolchain.FreeBSD()
        toolchain.Cygwin()


if __name__ == '__main__':
    unittest.main()
