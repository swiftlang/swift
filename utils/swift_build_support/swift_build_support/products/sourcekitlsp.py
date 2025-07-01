# swift_build_support/products/sourcekitlsp.py -------------------*- python -*-
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

import os

from build_swift.build_swift.constants import MULTIROOT_DATA_FILE_PATH

from . import cmark
from . import foundation
from . import libcxx
from . import libdispatch
from . import llbuild
from . import llvm
from . import product
from . import swift
from . import swiftpm
from . import xctest
from .. import shell
from .. import targets


class SourceKitLSP(product.Product):
    @classmethod
    def product_source_name(cls):
        return "sourcekit-lsp"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def is_swiftpm_unified_build_product(cls):
        return True

    def should_build(self, host_target):
        return True

    def _run_swift_syntax_dev_utils(self, host_target, command, arguments=[]):
        sourcekit_lsp_dev_utils = os.path.join(self.source_dir, 'SourceKitLSPDevUtils')

        run_cmd = [
            os.path.join(self.install_toolchain_path(host_target), "bin", "swift"),
            'run',
            '--package-path', sourcekit_lsp_dev_utils,
            'sourcekit-lsp-dev-utils',
            command,
        ] + arguments

        env = dict(os.environ)
        env["SWIFTCI_USE_LOCAL_DEPS"] = "1"

        shell.call(run_cmd, env=env)

    def _for_each_host_target(self, base_target, body):
        body(base_target)

        # For Darwin host, 'build' is only called for the builder.
        # Manually iterate the cross compile hosts.
        if self.has_cross_compile_hosts() and self.is_darwin_host(base_target):
            for target in self.args.cross_compile_hosts:
                body(target)

    async def build(self, host_target):
        if self.args.sourcekitlsp_verify_generated_files:
            self._run_swift_syntax_dev_utils(
                host_target, 'verify-config-schema')

        self._for_each_host_target(
            host_target,
            lambda target: self.run_build_script_helper('build', host_target, target)
        )

    def should_test(self, host_target):
        return self.args.test_sourcekitlsp

    def test(self, host_target):
        self.run_build_script_helper('test', host_target, host_target)

    def should_install(self, host_target):
        return self.args.install_sourcekitlsp

    def install(self, host_target):
        self._for_each_host_target(
            host_target,
            lambda target: self.run_build_script_helper('install', host_target, target)
        )

    @classmethod
    def get_dependencies(cls):
        return [cmark.CMark,
                llvm.LLVM,
                libcxx.LibCXX,
                swift.Swift,
                libdispatch.LibDispatch,
                foundation.Foundation,
                xctest.XCTest,
                llbuild.LLBuild,
                swiftpm.SwiftPM]

    def run_build_script_helper(self, action, base_target, host_target):
        # base_target is the machine that's driving the build.
        # host_target is the target we are bulding for.
        script_path = os.path.join(
            self.source_dir, 'Utilities', 'build-script-helper.py')

        install_destdir = self.host_install_destdir(host_target)
        toolchain_path = self.native_toolchain_path(base_target)
        configuration = 'release' if self.is_release() else 'debug'
        helper_cmd = [
            script_path,
            action,
            '--package-path', self.source_dir,
            '--build-path', self.build_dir,
            '--configuration', configuration,
            '--toolchain', toolchain_path,
            '--ninja-bin', self.toolchain.ninja,
            '--multiroot-data-file', MULTIROOT_DATA_FILE_PATH,
        ]
        if self.args.verbose_build:
            helper_cmd.append('--verbose')

        if self.args.test_sourcekitlsp_sanitize_all:
            helper_cmd.append('--sanitize-all')
        elif self.args.enable_asan:
            helper_cmd += ['--sanitize', 'address']
        elif self.args.enable_ubsan:
            helper_cmd += ['--sanitize', 'undefined']
        elif self.args.enable_tsan:
            helper_cmd += ['--sanitize', 'thread']

        if self.has_cross_compile_hosts():
            helper_cmd += ['--cross-compile-host', host_target]
            if self.is_cross_compile_target(host_target) and \
                    not self.is_darwin_host(host_target):
                build_toolchain_path = install_destdir + self.args.install_prefix
                resource_dir = '%s/lib/swift' % build_toolchain_path
                helper_cmd += [
                    '--cross-compile-config',
                    targets.StdlibDeploymentTarget.get_target_for_name(host_target)
                    .platform
                    .swiftpm_config(self.args, output_dir=build_toolchain_path,
                                    swift_toolchain=toolchain_path,
                                    resource_path=resource_dir)
                ]

        if action == 'install':
            helper_cmd += ['--prefix', install_destdir + self.args.install_prefix]

        shell.call(helper_cmd)
