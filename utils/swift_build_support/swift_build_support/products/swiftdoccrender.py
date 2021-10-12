# swift_build_support/products/swiftdoccrender.py ---------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os

from . import product
from . import swiftdocc
from .. import shell


class SwiftDocCRender(product.Product):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-docc-render"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def is_swiftpm_unified_build_product(cls):
        return False

    def run_build_script_helper(self, action, host_target, additional_params=[]):
        script_path = os.path.join(
            self.source_dir, 'build-script-helper.py')

        helper_cmd = [
            script_path,
            action
        ]
        if self.args.verbose_build:
            helper_cmd.append('--verbose')
        helper_cmd.extend(additional_params)

        shell.call(helper_cmd)

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        self.run_build_script_helper('build', host_target)

    def should_test(self, host_target):
        return self.args.test_swiftdoccrender

    def test(self, host_target):
        self.run_build_script_helper('test', host_target)

    def should_install(self, host_target):
        return self.args.install_swiftdoccrender

    def install(self, host_target):
        install_dir = swiftdocc.SwiftDocC.get_render_install_destdir(
            self.install_toolchain_path(host_target))
        additional_params = ['--install-dir', install_dir]
        self.run_build_script_helper('install', host_target, additional_params)

    @classmethod
    def get_dependencies(cls):
        return [swiftdocc.SwiftDocC]
