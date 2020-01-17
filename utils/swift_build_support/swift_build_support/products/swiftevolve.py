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

from . import skstresstester


class SwiftEvolve(skstresstester.SKStressTester):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-stress-tester"

    def package_name(self):
        return 'SwiftEvolve'

    # Inherit the entire build configuration from the SourceKit stress tester

    def should_build(self, host_target):
        return True

    def should_test(self, host_target):
        return self.args.test_swiftevolve

    def should_install(self, host_target):
        return self.args.install_swiftevolve
