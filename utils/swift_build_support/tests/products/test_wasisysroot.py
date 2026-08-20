# test_wasisysroot.py -----------------------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import argparse
import contextlib
import os
import shutil
import tempfile
import unittest
from io import StringIO

from swift_build_support.products import WASISysroot
from swift_build_support.targets import StdlibDeploymentTarget
from swift_build_support.toolchain import host_toolchain
from swift_build_support.workspace import Workspace


class WASISysrootTestCase(unittest.TestCase):
    def setUp(self):
        self.workspace = Workspace(
            source_root=os.path.realpath(tempfile.mkdtemp()),
            build_root=os.path.realpath(tempfile.mkdtemp()))
        self.host = StdlibDeploymentTarget.host_target().name
        self.toolchain = host_toolchain()
        self.tools_dir = os.path.realpath(tempfile.mkdtemp())

    def tearDown(self):
        for path in (self.workspace.source_root, self.workspace.build_root,
                     self.tools_dir):
            shutil.rmtree(path, ignore_errors=True)

    def _resolve(self, tools_dir=None, dry_run=False, build_wasmkit=False):
        args = argparse.Namespace(
            wasi_libc_component_tools_path=tools_dir,
            dry_run=dry_run,
            build_wasmkit=build_wasmkit)
        product = WASISysroot(
            args=args, toolchain=self.toolchain,
            source_dir=self.workspace.source_root,
            build_dir=os.path.join(self.workspace.build_root,
                                   'wasisysroot-' + self.host))
        return product._wasm_component_tool_paths(self.host)

    def _make_tool(self, name):
        path = os.path.join(self.tools_dir, name)
        open(path, 'w').close()
        return path

    def test_supplied_dir_prefers_wasmkit_names(self):
        embed = self._make_tool('wasmkit')
        link = self._make_tool('wasmkit-component-ld')
        self._make_tool('wasm-tools')
        self._make_tool('wasm-component-ld')
        self.assertEqual(self._resolve(tools_dir=self.tools_dir), (embed, link))

    def test_supplied_dir_falls_back_to_current_names(self):
        embed = self._make_tool('wasm-tools')
        link = self._make_tool('wasm-component-ld')
        self.assertEqual(self._resolve(tools_dir=self.tools_dir), (embed, link))

    def test_supplied_dir_resolves_each_tool_independently(self):
        embed = self._make_tool('wasmkit')
        link = self._make_tool('wasm-component-ld')
        self.assertEqual(self._resolve(tools_dir=self.tools_dir), (embed, link))

    def test_supplied_dir_with_colon_in_path(self):
        colon_dir = os.path.join(self.workspace.build_root, 'a:b')
        os.makedirs(colon_dir)
        embed = os.path.join(colon_dir, 'wasmkit')
        link = os.path.join(colon_dir, 'wasmkit-component-ld')
        for path in (embed, link):
            open(path, 'w').close()
        self.assertEqual(self._resolve(tools_dir=colon_dir), (embed, link))

    def test_supplied_dir_missing_one_tool_exits(self):
        self._make_tool('wasmkit')  # link driver absent
        with contextlib.redirect_stderr(StringIO()):
            with self.assertRaises(SystemExit):
                self._resolve(tools_dir=self.tools_dir)

    def test_dry_run_present_tools_still_resolve(self):
        embed = self._make_tool('wasm-tools')
        link = self._make_tool('wasm-component-ld')
        self.assertEqual(
            self._resolve(tools_dir=self.tools_dir, dry_run=True), (embed, link))

    def test_dry_run_missing_tools_uses_placeholder(self):
        self.assertEqual(
            self._resolve(tools_dir=self.tools_dir, dry_run=True),
            (os.path.join(self.tools_dir, 'wasmkit'),
             os.path.join(self.tools_dir, 'wasmkit-component-ld')))

    def test_wasmkit_branch(self):
        wasmkit_dir = os.path.join(self.workspace.build_root,
                                   'wasmkit-' + self.host)
        self.assertEqual(
            self._resolve(build_wasmkit=True),
            (os.path.join(wasmkit_dir, 'bin', 'wasmkit'),
             os.path.join(wasmkit_dir, 'bin', 'wasmkit-component-ld')))

    def test_neither_returns_none(self):
        self.assertEqual(self._resolve(), (None, None))


if __name__ == '__main__':
    unittest.main()
