
from __future__ import absolute_import

import os.path

from .. import shell

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
                "--foundation=" + os.path.join(
                    self.foundation_build.build_dir, "Foundation"),
                "--xctest=" + self.xctest_build.build_dir]
        return build_cmd
            
    def build(self):
        shell.invoke(self.bootstrap_command())

    def test(self):
        shell.invoke(self.bootstrap_command() + ["test", ])

    def install(self):
        prefix = os.path.join(self.args.install_destdir,
                              self.args.install_prefix.lstrip('/'))
        shell.invoke(self.bootstrap_command() + ["--prefix=" + prefix,
                                                 "install"])

