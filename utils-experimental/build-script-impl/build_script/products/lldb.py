# build_script/products/lldb.py ---------------------------------*- python -*-
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
LLDB builder
"""
# ----------------------------------------------------------------------------

import os.path
import datetime

from .. import shell
from ..xcodebuild import Xcodebuild, XcodebuildOptions
from ..cmake import CMakeOptions
from ..host import host
from ..exceptions import BuildError
from ..utils import printf


class LLDB(object):
    source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('lldb')
        if cls.source_dir is None:
            raise BuildError("Couldn't find lldb source directory.")

    def __init__(self,
                 deployment_target,
                 target_build_dir,
                 target_install_destdir,
                 cmark_build,
                 llvm_build,
                 swift_build,
                 cmake,
                 args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.install_destdir = target_install_destdir
        self.cmark_build = cmark_build
        self.llvm_build = llvm_build
        self.swift_build = swift_build
        self.cmake = cmake

        self.args = args

    def configure(self):
        is_buildbot_build = (
            ('JENKINS_HOME' in os.environ) and
            ('JOB_NAME' in os.environ) and
            ('BUILD_NUMBER' in os.environ))
        if is_buildbot_build:
            is_buildbot_build = "1"
        else:
            is_buildbot_build = "0"

        build_date = datetime.date.today().strftime('"%Y-%m-%d"')

        options = None
        if host.is_darwin():
            options = XcodebuildOptions()
        else:
            options = CMakeOptions()
        define = options.define

        define('LLDB_PATH_TO_LLVM_SOURCE:PATH',
               self.llvm_build.source_dir)
        define('LLDB_PATH_TO_CLANG_SOURCE:PATH',
               self.llvm_build.clang_source_dir)
        define('LLDB_PATH_TO_SWIFT_SOURCE:PATH', self.swift_build.source_dir)
        define('LLDB_PATH_TO_LLVM_BUILD:PATH', self.llvm_build.build_dir)
        define('LLDB_PATH_TO_CLANG_BUILD:PATH', self.llvm_build.build_dir)
        define('LLDB_PATH_TO_SWIFT_BUILD:PATH', self.swift_build.build_dir)
        define('LLDB_PATH_TO_CMARK_BUILD:PATH', self.cmark_build.build_dir)
        define('LLDB_IS_BUILDBOT_BUILD:BOOL', is_buildbot_build)
        define('LLDB_BUILD_DATE:STRING', build_date)

        if host.is_darwin():
            # Set up flags to pass to xcodebuild

            if self.args.lldb_no_debugserver:
                define('DEBUGSERVER_DISABLE_CODESIGN', "1")
                define('DEBUGSERVER_DELETE_AFTER_BUILD', "1")
            if self.args.lldb_use_system_debugserver:
                define('DEBUGSERVER_USE_FROM_SYSTEM', "1")

            define('SYMROOT', self.build_dir)
            define('OBJROOT', self.build_dir)

            # xcodebuild doesn't have separate configure and build phases.
            # Store configuration options into myself.
            self.xcodebuild_options = options
            self.xcodebuild_config = "CustomSwift-" + self.args.lldb_build_type
        else:
            if (not self.args.reconfigure and
                    self.cmake.is_configured(self.build_dir)):
                return

            printf('--- Configuring LLDB ---')
            if self.args.use_gold_linker:
                define('CMAKE_EXE_LINKER_FLAGS:STRING', "-fuse-ld=gold")
                define('CMAKE_SHARED_LINKER_FLAGS:STRING', "-fuse-ld=gold")

            define('LLDB_ALLOW_STATIC_BINDINGS:BOOL', True)
            define('CMAKE_BUILD_TYPE:STRING', self.args.lldb_build_type)
            define('CMAKE_INSTALL_PREFIX:PATH', self.args.install_prefix)

            if len(self.args.lldb_extra_cmake_args) is not None:
                options += self.args.lldb_extra_cmake_args

            self.cmake.configure(
                source_dir=self.source_dir,
                build_dir=self.build_dir,
                options=options)

    def build(self):
        printf('--- Building LLDB ---')
        if host.is_darwin():
            xcodebuild = Xcodebuild()
            xcodebuild.build(
                project_dir=self.source_dir,
                target="desktop",
                configuration=self.xcodebuild_config,
                action="build",
                options=self.xcodebuild_options)
        else:
            self.cmake.build(
                build_dir=self.build_dir,
                build_targets=['all', ])

    def test(self):
        if self.deployment_target != self.args.host_target:
            return
        if self.args.skip_test_lldb:
            return
        printf('--- Running tests for LLDB ---')
        if host.is_darwin():
            lldb_bin_path = os.path.join(self.build_dir,
                                         self.args.lldb_build_type,
                                         "lldb")
        else:
            lldb_bin_path = os.path.join(self.build_dir, "bin", "lldb")

        results_dir = os.path.join(self.build_dir, 'test-results')
        if not os.path.exists(results_dir):
            shell.makedirs(results_dir)

        # Handle test results formatter
        if self.args.lldb_test_with_curses:
            # Setup the curses results formatter.
            lldb_formatter_opts = [
                '--results-formatter',
                'lldbsuite.test.curses_results.Curses',
                '--results-file',
                '/dev/stdout']
        else:
            lldb_formatter_opts = [
                '--results-formatter',
                'lldbsuite.test.xunit_formatter.XunitFormatter',
                '--results-file',
                os.path.join(results_dir, 'results.xml'),
                '-O--xpass=ignore']

        # Setup the xUnit results formatter.
        if not host.is_darwin():
            # On non-Darwin, we ignore skipped tests entirely
            # so that they don't pollute our xUnit results with
            # non-actionable content.
            lldb_formatter_opts += [
                '-O-ndsym', '-O-rdebugserver',
                r'-O-rlibc\\+\\+', '-O-rlong.running',
                '-O-rbenchmarks', '-O-rrequires.one?.of.darwin']

        # figure out which C/C++ compiler we should use for building test
        # inferiors.
        if self.args.lldb_test_cc == "host-toolchain":
            do_test_cc = self.args.host_cc
        elif self.args.lldb_test_cc is not None:
            do_test_cc = self.args.lldb_test_cc
        else:
            do_test_cc = self.llvm_build.clang_bin_path

        test_cmd = [os.path.join(self.source_dir, 'test', 'dotest.py'), ]
        test_cmd += ['--executable', lldb_bin_path]
        test_cmd += ['--rerun-all-issues']
        test_cmd += ['-C', do_test_cc]
        test_cmd += lldb_formatter_opts

        env = [
            ("SWIFTCC", self.swift_build.swiftc_bin_path),
            ("SWIFTLIBS", self.swift_build.swiftlib_path)]

        with shell.pushd(results_dir):
            shell.invoke(test_cmd, env=env)

    def install(self):
        if not self.args.install_lldb:
            return

        printf("--- Installing LLDB ---")
        if host.is_darwin():
            xcodebuild = Xcodebuild()
            install_options = XcodebuildOptions(
                # FIXME: We should use self.install_destdir instead.
                ("DSTROOT", self.args.install_destdir),
                ("LLDB_TOOLCHAIN_PREFIX", self.args.toolchain_prefix))
            xcodebuild.build(
                project_dir=self.source_dir,
                target="toolchain",
                configuration=self.xcodebuild_config,
                action="install",
                options=self.xcodebuild_options + install_options)
        else:
            self.cmake.install(
                dest_dir=self.install_destdir,
                build_dir=self.build_dir)
