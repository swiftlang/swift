# swift_build_support/products/cmark.py -------------------------*- python -*-
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

from . import product
import sys

class CMark(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        """is_build_script_impl_product -> bool

        Whether this product is produced by build-script-impl.
        """
        return False

    # This is the root of the build-graph, so it doesn't have any dependencies.
    @classmethod
    def get_dependencies(cls):
        return []

    def should_build(self, host_target):
        """should_build() -> Bool

        Whether or not this product should be built with the given arguments.
        """
        return True

    def build(self, host_target):
        """build() -> void

        Perform the build, for a non-build-script-impl product.
        """
        self.cmake_options.define('CMAKE_BUILD_TYPE:STRING',
                                  self.args.cmark_build_variant)

        self.build_with_cmake(["all"], self.args.cmark_build_variant, [])

    def should_test(self, host_target):
        """should_test() -> Bool

        Whether or not this product should be tested with the given arguments.
        """
        return self.args.test

    def should_install(self, host_target):
        """should_install() -> Bool

        Whether or not this product should be installed with the given
        arguments.
        """
        return self.args.install_all
