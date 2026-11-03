# swift_build_support/products/swift_testing_harness.py ----------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2024 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os

from build_swift.build_swift.constants import MULTIROOT_DATA_FILE_PATH

from . import product
from . import swift
from . import swiftpm
from . import swift_testing
from .. import shell


class SwiftTestingHarness(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def product_source_name(cls):
        return "swift-testing/Sources/Harness"

    @classmethod
    def get_dependencies(cls):
        return [swift.Swift,
                swiftpm.SwiftPM,
                swift_testing.SwiftTesting]

    def should_clean(self, host_target):
        # Workaround for 'swift-testing' not detecting compiler/stdlib changes.
        return True

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        self.run_build_script_helper('build', host_target)

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return self.args.install_swift_testing_harness

    def install(self, host_target):
        self._for_each_host_target(
            host_target,
            lambda target: self.run_build_script_helper('install', host_target, target)
        )

    def run_build_script_helper(self, action, host_target, additional_params=[]):
        script_path = os.path.join(
            self.source_dir, 'build-script-helper.py')

        helper_cmd = [
            script_path,
            action,
            '--toolchain', self.install_toolchain_path(host_target),
            '--configuration', self.configuration(),
            '--build-path', self.build_dir,
            '--multiroot-data-file', MULTIROOT_DATA_FILE_PATH,
        ]

        install_destdir = self.host_install_destdir(host_target)
        toolchain_path = self.native_toolchain_path(host_target)

        # Pass Cross compile host info unless we're testing.
        # It doesn't make sense to run tests of the cross compile host.
        if self.has_cross_compile_hosts() and action != 'test':
            if self.is_darwin_host(host_target):
                if len(self.args.cross_compile_hosts) != 1:
                    raise RuntimeError("Cross-Compiling swift-testing-harness to multiple " +
                                       "targets is not supported")
                helper_cmd += ['--cross-compile-host', self.args.cross_compile_hosts[0]]
            elif self.is_cross_compile_target(host_target):
                helper_cmd.extend(['--cross-compile-host', host_target])
                build_toolchain_path = install_destdir + self.args.install_prefix
                resource_dir = f'{build_toolchain_path}/lib/swift'
                cross_compile_config = targets.StdlibDeploymentTarget \
                    .get_target_for_name(host_target) \
                    .platform \
                    .swiftpm_config(
                        self.args,
                        output_dir=build_toolchain_path,
                        swift_toolchain=toolchain_path,
                        resource_path=resource_dir
                    )
                helper_cmd += ['--cross-compile-config', cross_compile_config]

        if self.args.verbose_build:
            helper_cmd.append('--verbose')
        helper_cmd.extend(additional_params)

        shell.call(helper_cmd)
