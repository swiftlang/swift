# swift_build_support/products/swiftpm.py -----------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os

from . import product
from .. import shell

class SwiftTSC(product.Product):
    @classmethod
    def product_source_name(cls):
        return "swift-tools-support-core"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return True

    def run_build_script(self, host_target):
        script_path = os.path.join(
            self.source_dir, 'Utilities', 'build-script-helper.py')
        toolchain_path = self.install_toolchain_path()
        swiftc = os.path.join(toolchain_path, "usr", "bin", "swiftc")

        helper_cmd = [script_path, "build"]
        helper_cmd += [
            "--swiftc-path", swiftc,
            "--cmake-path", self.toolchain.cmake,
            "--ninja-path", self.toolchain.ninja,
            "--build-dir", self.build_dir,
        ]
        shell.call(helper_cmd)

    def build(self, host_target):
        self.run_build_script(host_target)

    def should_test(self, host_target):
        # TSC is tested with SwiftPM.
        return False

    def test(self, host_target):
        pass

    def should_install(self, host_target):
        return False

    def install(self, host_target):
        pass
