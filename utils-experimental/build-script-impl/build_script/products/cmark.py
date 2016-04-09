# build_script/products/cmark.py --------------------------------*- python -*-
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
CMark builder
"""
# ----------------------------------------------------------------------------

import os.path
from ..exceptions import BuildError
from .. import targets
from ..cmake import CMakeOptions
from ..utils import printf

class CMark(object):

    source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('cmark')
        if cls.source_dir is None:
            raise BuildError("Couldn't find cmark source directory.")

    def __init__(self,
                 deployment_target,
                 target_build_dir,
                 target_install_destdir,
                 cmake,
                 args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.install_destdir = target_install_destdir
        self.cmake = cmake
        self.args = args

    @property
    def library_dir(self):
        if self.cmake.generator == "Xcode":
            return os.path.join(self.build_dir, 'src',
                                self.args.cmark_build_type)
        else:
            return os.path.join(self.build_dir, 'src')

    def _c_flags(self):
        cflags = targets.common_cflags(self.deployment_target)
        if self.cmake.is_release_build_type(self.args.cmark_build_type):
            cflags += ('-fno-stack-protector', )

    def configure(self):
        if (not self.args.reconfigure and
            os.path.exists(os.path.join(self.build_dir, 'CMakeCache.txt'))):
            return

        cmake_options = CMakeOptions()
        define = cmake_options.define

        if self.args.use_gold_linker:
            define('CMAKE_EXE_LINKER_FLAGS:STRING', "-fuse-ld=gold")
            define('CMAKE_SHARED_LINKER_FLAGS:STRING', "-fuse-ld=gold")

        define('CMAKE_BUILD_TYPE:STRING', self.args.cmark_build_type)

        if targets.is_darwin_type(self.deployment_target):
            c_flags = ' '.join(self._c_flags())
            xcrun_sdk_name = targets.xcrun_sdk_name(self.deployment_target)
            xcrun_sdk_path = host.xcrun_sdk_path(xcrun_sdk_name)
            define('CMAKE_C_FLAGS', c_flags)
            define('CMAKE_CXX_FLAGS', c_flags)
            define('CMAKE_OSX_SYSROOT:PATH', xcrun_sdk_name)
            if targets.is_osx(self.deployment_target):
                define('CMAKE_OSX_DEPLOYMENT_TARGET',
                        self.args.darwin_deployment_version_osx)

        # Do configure
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
        self.cmake.build(
            build_targets = ['all', ],
            build_dir=self.build_dir,
            config_opts=self._cmake_config_opts())

    def test(self):
        if self.args.skip_test_cmark:
            return

        printf("--- Building tests for CMark ---")
        self.cmake.build(
            build_targets=["api_test", ],
            build_dir=self.build_dir,
            config_opts=self._cmake_config_opts())

        printf("--- Running tests for CMark ---")
        results_targets = ["test", ]
        if self.args.cmake_generator == 'Xcode':
            results_targets = ["RUN_TESTS", ]
        self.cmake.test(
            test_targets=["test", ],
            build_dir=self.build_dir,
            config_opts=self._cmake_config_opts())

    def install(self):
        if not self.args.install_cmark:
            return
        printf("--- Installing CMark ---")
        self.cmake.install(
            dest_dir=self.install_destdir,
            build_dir=self.build_dir)
