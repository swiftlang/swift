# swift_build_support/products/earlyswiftsyntax.py --------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

from . import cmake_product
from .. import toolchain


# SwiftSyntax is a Swift module used to parse and manipulate Swift syntax.  This
# build product is a "Special" SwiftSyntax that gets built with the host
# toolchain that can be linked into the Swift compiler itself, hence it does not
# depend on any other build product of `build-script`.
class EarlySwiftSyntax(cmake_product.CMakeProduct):
    @classmethod
    def product_source_name(cls):
        return "swift-syntax"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return True

    def should_build(self, host_target):
        if self.args.build_early_swiftsyntax:
            if toolchain.host_toolchain().find_tool("swift") is None:
                warn_msg = 'Host toolchain could not locate a '\
                           'compiler to build early swift-syntax.'
                print('-- Warning: {}', warn_msg)
                return False
            else:
                return True
        return False

    @classmethod
    def get_dependencies(cls):
        return []

    def build(self, host_target):
        self.cmake_options.define('CMAKE_BUILD_TYPE:STRING',
                                  self.args.swift_build_variant)
        self.cmake_options.define('BUILD_SHARED_LIBS:STRING', 'NO')

        self.generate_toolchain_file_for_darwin_or_linux(host_target)

        self.build_with_cmake(["all"], self.args.swift_build_variant, [])

    def should_test(self, host_target):
        # The normal SwiftSyntax target runs tests through SwiftPM.
        return False

    def test(self, host_target):
        pass

    def should_install(self, host_target):
        # This product is for the swift-syntax used with the build-directory compiler.
        # If a toolchain install is required, please use the SwiftSyntax (no 'Early')
        # product with `--swift-syntax --install-swift-syntax`.
        return False

    @classmethod
    def is_ignore_install_all_product(cls):
        # Ensures that `install_all` setting triggered by `--infer` does not
        # affect products which specify `is_ignore_install_all_product` as
        # True. This is useful for products which should not be installed into the
        # toolchain (corresponding build products that use the just-built
        # toolchain are the products that get installed, e.g. `swiftsyntax` to
        # `earlyswiftsyntax`).
        return True
