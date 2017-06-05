# tests/test_workspace.py ---------------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import argparse
import itertools
import os
import shutil
import tempfile
import unittest

from swift_build_support.workspace import (
    Workspace,
    compute_build_subdir,
)


class WorkspaceTestCase(unittest.TestCase):

    def test_workspace(self):
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, 'foo'))

        workspace = Workspace(source_root=tmpdir1,
                              build_root=tmpdir2)

        self.assertEqual(workspace.source_root, tmpdir1)
        self.assertEqual(workspace.build_root, tmpdir2)

        # source_dir
        self.assertEqual(workspace.source_dir('foo'),
                         os.path.join(tmpdir1, 'foo'))

        # build_dir
        self.assertEqual(workspace.build_dir('target', 'product'),
                         os.path.join(tmpdir2, 'product-target'))

        shutil.rmtree(tmpdir1)
        shutil.rmtree(tmpdir2)


class ComputeBuildSubdirTestCase(unittest.TestCase):

    def create_basic_args(self, generator, variant, assertions,
                          enable_asan=False, enable_ubsan=False,
                          enable_tsan=False):
        return argparse.Namespace(
            cmake_generator=generator,
            cmark_build_variant=variant,
            llvm_build_variant=variant,
            swift_build_variant=variant,
            swift_stdlib_build_variant=variant,
            swift_analyze_code_coverage="false",
            cmark_assertions=assertions,
            llvm_assertions=assertions,
            swift_assertions=assertions,
            swift_stdlib_assertions=assertions,
            enable_asan=enable_asan,
            enable_ubsan=enable_ubsan,
            enable_tsan=enable_tsan)

    def test_Ninja_ReleaseAssert_asan(self):  # noqa (N802 function name should be lowercase)
        args = self.create_basic_args(
            "Ninja", variant="Release", assertions=True, enable_asan=True)
        self.assertEqual(compute_build_subdir(args),
                         "Ninja-ReleaseAssert+asan")

    def test_Ninja_ReleaseAssert_ubsan(self):  # noqa (N802 function name should be lowercase)
        args = self.create_basic_args(
            "Ninja", variant="Release", assertions=True, enable_ubsan=True)
        self.assertEqual(compute_build_subdir(args),
                         "Ninja-ReleaseAssert+ubsan")

    def test_Ninja_ReleaseAssert_tsan(self):  # noqa (N802 function name should be lowercase)
        args = self.create_basic_args(
            "Ninja", variant="Release", assertions=True, enable_tsan=True)
        self.assertEqual(compute_build_subdir(args),
                         "Ninja-ReleaseAssert+tsan")

    def test_Ninja_ReleaseAssert(self):  # noqa (N802 function name should be lowercase)
        # build-script -R
        args = self.create_basic_args(
            "Ninja", variant="Release", assertions=True)
        self.assertEqual(compute_build_subdir(args),
                         "Ninja-ReleaseAssert")

    def test_Ninja_Release(self):  # noqa (N802 function name should be lowercase)
        # build-script -R --no-assertions
        args = self.create_basic_args(
            "Ninja", variant="Release", assertions=False)
        self.assertEqual(compute_build_subdir(args),
                         "Ninja-Release")

    def test_Ninja_Release_stdlib_ReleaseAssert(self):  # noqa (N802 function name should be lowercase)
        # build-script -R --no-assertions --swift-stdlib-assertions
        args = self.create_basic_args(
            "Ninja", variant="Release", assertions=False)
        args.swift_stdlib_assertions = True
        self.assertEqual(compute_build_subdir(args),
                         "Ninja-Release+stdlib-ReleaseAssert")

    def test_Ninja_mixed(self):  # noqa (N802 function name should be lowercase)
        # build-script -R --no-assertions
        #     --llvm-build-variant=RelWithDebInfo
        #     --swift-analyze-code-coverage="merged"
        #     --swift-stdlib-assertions
        args = self.create_basic_args(
            "Ninja", variant="Release", assertions=False)
        args.llvm_build_variant = "RelWithDebInfo"
        args.swift_analyze_code_coverage = "merged"
        args.swift_stdlib_assertions = True
        self.assertEqual(compute_build_subdir(args),
                         "Ninja+cmark-Release+llvm-RelWithDebInfo"
                         "+swift-ReleaseCoverage+stdlib-ReleaseAssert")

    def test_Unix_Makefiles_ReleaseAssert(self):  # noqa (N802 function name should be lowercase)
        # build-script -R -m
        args = self.create_basic_args(
            "Unix Makefiles", variant="Release", assertions=True)
        self.assertEqual(compute_build_subdir(args),
                         "Unix_Makefiles-ReleaseAssert")

    def test_all_combinations_are_unique(self):
        productions = itertools.product(
            ["Release", "Debug"],        # cmark_build_variant
            ["Release", "Debug"],        # llvm_build_variant
            ["Release", "Debug"],        # swift_build_variant
            ["Release", "Debug"],        # swift_stdlib_build_variant
            ["false", "true"],           # swift_analyze_code_coverage
            [True, False],               # cmark_assertions
            [True, False],               # llvm_assertions
            [True, False],               # swift_assertions
            [True, False],               # swift_stdlib_assertions
        )
        keys = [
            "cmark_build_variant",
            "llvm_build_variant",
            "swift_build_variant",
            "swift_stdlib_build_variant",
            "swift_analyze_code_coverage",
            "cmark_assertions",
            "llvm_assertions",
            "swift_assertions",
            "swift_stdlib_assertions",
        ]

        def generate():
            for c in productions:
                args = argparse.Namespace(cmake_generator="Ninja",
                                          enable_asan=False,
                                          enable_ubsan=False,
                                          enable_tsan=False)
                for key, val in zip(keys, c):
                    setattr(args, key, val)
                yield compute_build_subdir(args)

        seen = set()
        for line in generate():
            self.assertIsInstance(line, str)
            self.assertNotIn(line, seen)
            seen.add(line)
        self.assertEqual(len(seen), 1 << 9)  # Iterated all productions.
