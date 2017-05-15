# build_script/products/swiftpm.py ------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------
"""
Swift Package Manager builder
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import os.path

from .. import shell
from ..exceptions import BuildError
from ..host import host
from ..utils import printf


# FIXME: consider cross tools deployment targets
class SwiftPM(object):
    source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('swiftpm')
        if cls.source_dir is None:
            raise BuildError("Couldn't find swiftpm source directory.")

    def __init__(self,
                 deployment_target,
                 target_build_dir,
                 target_install_destdir,
                 swift_build,
                 llbuild_build,
                 foundation_build,
                 xctest_build,
                 args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.install_destdir = target_install_destdir

        self.swift_build = swift_build
        self.llbuild_build = llbuild_build
        self.foundation_build = foundation_build
        self.xctest_build = xctest_build

        self.args = args

    def configure(self):
        printf("--- Configuring Swift Package Manager ---")
        if self.llbuild_build is None:
            raise RuntimeError(
                "Error: Cannot build swiftpm without llbuild"
                " (swift-build-tool).")

    def bootstrap_command(self):
        build_cmd = [os.path.join(self.source_dir, 'Utilities', 'bootstrap')]

        if self.deployment_target == 'macosx-x86_64':
            build_cmd += ['--sysroot=' + host.sdk_path("macosx"), ]
        if self.args.verbose_build:
            build_cmd += ["-v", ]
        build_cmd += [
            "--swiftc=" + self.swift_build.swiftc_bin_path,
            "--sbt=" + self.llbuild_build.swift_build_tool_bin_path,
            "--build=" + self.build_dir]

        if self.foundation_build and self.xctest_build:
            build_cmd += [
                "--foundation=" + self.foundation_build.foundation_path,
                "--xctest=" + self.xctest_build.build_dir]
        return build_cmd

    def build(self):
        printf("--- Building Swift Package Manager ---")
        shell.invoke(self.bootstrap_command())

    def test(self):
        if self.deployment_target != self.args.host_target:
            return
        if self.args.skip_test_swiftpm:
            return
        printf("--- Running tests for Swift Package Manager ---")
        shell.invoke(self.bootstrap_command() + ["test", ])

    def install(self):
        if not self.args.install_swiftpm:
            return
        printf("--- Installing Swift Package Manager ---")

        # FIXME: We should use self.install_destdir instead.
        prefix = os.path.join(self.args.install_destdir,
                              self.args.install_prefix.lstrip('/'))
        shell.invoke(self.bootstrap_command() + ["--prefix=" + prefix,
                                                 "install"])
