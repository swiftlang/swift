# swift_build_support/products/swiftevolve.py --------------------*- python -*-
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

from . import skstresstester
from .. import shell


class SwiftEvolve(skstresstester.SKStressTester):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-stress-tester"

    @classmethod
    def is_swiftpm_unified_build_product(cls):
        return False

    def package_name(self):
        return 'SwiftEvolve'

    # Copy of the build-script-helper invocation without the multiroot data 
    # file. Remove again once SwiftEvolve also builds in the unified build.
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
            # There might have been a Package.resolved created by other builds
            # or by the package being opened using Xcode. Discard that and
            # reset the dependencies to be local.
            '--update'
        ]
        if self.args.verbose_build:
            helper_cmd.append('--verbose')
        helper_cmd.extend(additional_params)

        shell.call(helper_cmd)

    # Inherit the entire build configuration from the SourceKit stress tester

    def should_build(self, host_target):
        return True

    def should_test(self, host_target):
        return self.args.test_swiftevolve

    def should_install(self, host_target):
        return self.args.install_swiftevolve
