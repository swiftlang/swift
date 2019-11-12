
# swift_build_support/products/skstresstester.py -----------------*- python -*-
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
import platform

from . import product
from .. import multiroot_data_file
from .. import shell


class SKStressTester(product.Product):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-stress-tester"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_swiftpm_unified_build_product(cls):
        return True

    def package_name(self):
        return 'SourceKitStressTester'

    def run_build_script_helper(self, action, additional_params=[]):
        script_path = os.path.join(
            self.source_dir, 'build-script-helper.py')

        configuration = 'release' if self.is_release() else 'debug'

        helper_cmd = [
            script_path,
            action,
            '--package-dir', self.package_name(),
            '--toolchain', self.install_toolchain_path(),
            '--config', configuration,
            '--build-dir', self.build_dir,
            '--multiroot-data-file', multiroot_data_file.path(),
            # There might have been a Package.resolved created by other builds
            # or by the package being opened using Xcode. Discard that and
            # reset the dependencies to be local.
            '--update'
        ]
        if self.args.verbose_build:
            helper_cmd.append('--verbose')
        helper_cmd.extend(additional_params)

        shell.call(helper_cmd)

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        if platform.system() != 'Darwin':
            raise RuntimeError("Unable to build {product} on a platform other "
                               "than Darwin".format(
                                   product=self.package_name()))

        self.run_build_script_helper('build')

    def should_test(self, host_target):
        return self.args.test_skstresstester

    def test(self, host_target):
        self.run_build_script_helper('test')

    def should_install(self, host_target):
        return self.args.install_skstresstester

    def install(self, host_target):
        install_prefix = self.args.install_destdir + self.args.install_prefix
        self.run_build_script_helper('install', [
            '--prefix', install_prefix
        ])
