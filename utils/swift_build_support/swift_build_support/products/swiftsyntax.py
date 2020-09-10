# swift_build_support/products/swiftsyntax.py --------------------*- python -*-
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

from . import product
from .. import shell


class SwiftSyntax(product.Product):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-syntax"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_swiftpm_unified_build_product(cls):
        return True

    def run_swiftsyntax_build_script(self, target, additional_params=[]):
        llvm_build_dir = os.path.join(self.build_dir, '..', 'llvm-' + target)
        llvm_build_dir = os.path.realpath(llvm_build_dir)

        script_path = os.path.join(self.source_dir, 'build-script.py')

        build_cmd = [
            script_path,
            '--build-dir', self.build_dir,
            '--multiroot-data-file', MULTIROOT_DATA_FILE_PATH,
            '--toolchain', self.install_toolchain_path(target),
            '--filecheck-exec', os.path.join(llvm_build_dir, 'bin',
                                             'FileCheck'),
        ]

        if self.is_release():
            build_cmd.append('--release')

        if self.args.swiftsyntax_verify_generated_files:
            build_cmd.append('--verify-generated-files')

        build_cmd.extend(additional_params)

        if self.args.verbose_build:
            build_cmd.append('--verbose')

        shell.call(build_cmd)

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        self.run_swiftsyntax_build_script(target=host_target)

    def should_test(self, host_target):
        return self.args.test_swiftsyntax

    def test(self, host_target):
        self.run_swiftsyntax_build_script(target=host_target,
                                          additional_params=['--test'])

    def should_install(self, host_target):
        return self.args.install_swiftsyntax

    def install(self, target_name):
        install_prefix = self.args.install_destdir + self.args.install_prefix

        dylib_dir = os.path.join(install_prefix, 'lib')

        additional_params = [
            '--dylib-dir', dylib_dir,
            '--install'
        ]

        self.run_swiftsyntax_build_script(target=target_name,
                                          additional_params=additional_params)
