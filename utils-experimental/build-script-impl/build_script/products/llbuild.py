# build_script/products/llbuild.py ------------------------------*- python -*-
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
llbuild builder
"""
# ----------------------------------------------------------------------------

import os.path
from ..exceptions import BuildError
from ..cmake import CMakeOptions
from ..utils import printf


class LLBuild(object):
    source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('llbuild')
        if cls.source_dir is None:
            raise BuildError("Couldn't find llbuild source directory.")

    def __init__(self,
                 deployment_target,
                 target_build_dir,
                 target_install_destdir,
                 llvm_build,
                 cmake,
                 args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.install_destdir = target_install_destdir
        self.llvm_build = llvm_build
        self.cmake = cmake

        self.args = args

    def _bin_dir(self):
        if self.args.cmake_generator == 'Xcode':
            return os.path.join(self.build_dir,
                                'bin', self.llbuild_build_type)
        else:
            return os.path.join(self.build_dir, 'bin')

    @property
    def swift_build_tool_bin_path(self):
        return os.path.join(self._bin_dir(), 'swift-build-tool')

    def configure(self):
        if (not self.args.reconfigure and
                self.cmake.is_configured(self.build_dir)):
            return

        printf("--- Configuring llbuild ---")
        cmake_options = CMakeOptions()
        define = cmake_options.define

        if self.args.use_gold_linker:
            define('CMAKE_EXE_LINKER_FLAGS:STRING', "-fuse-ld=gold")
            define('CMAKE_SHARED_LINKER_FLAGS:STRING', "-fuse-ld=gold")

        define('CMAKE_INSTALL_PREFIX:PATH', self.args.install_prefix)
        define('CMAKE_BUILD_TYPE:STRING', self.args.llbuild_build_type)
        define('LLVM_ENABLE_ASSERTIONS:BOOL',
               self.args.llbuild_enable_assertions)
        define('LIT_EXECUTABLE:PATH', self.llvm_build.lit_bin_path)
        define('FILECHECK_EXECUTABLE:PATH',
               self.llvm_build.filecheck_bin_path)

        self.cmake.configure(
            source_dir=self.source_dir,
            build_dir=self.build_dir,
            options=cmake_options)

    def _cmake_config_opts(self):
        config_opts = []
        if self.args.cmake_generator == 'Xcode':
            config_opts += [
                '--config', self.args.cmark_build_type]
        return config_opts

    def build(self):
        printf("--- Building llbuild ---")
        self.cmake.build(
            build_dir=self.build_dir,
            build_targets=['all', ],
            config_opts=self._cmake_config_opts())

    def test(self):
        if self.deployment_target != self.args.host_target:
            return
        if self.args.skip_test_llbuild:
            return

        printf("--- Running tests for llbuild ---")
        self.cmake.test(
            build_dir=self.build_dir,
            test_targets=['test', ],
            config_opts=self._cmake_config_opts())

    def install(self):
        if not self.args.install_llbuild:
            return

        printf("--- Installing llbuild ---")
        self.cmake.install(
            build_dir=self.build_dir,
            dest_dir=self.install_destdir,
            install_targets=["install-swift-build-tool", ])
