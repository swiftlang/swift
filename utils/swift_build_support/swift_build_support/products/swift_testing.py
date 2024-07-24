# swift_build_support/products/swift_testing.py -----------------*- python -*-
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

from . import cmake_product
from . import swift


class SwiftTesting(cmake_product.CMakeProduct):
    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def product_source_name(cls):
        return "swift-testing"

    @classmethod
    def get_dependencies(cls):
        return [swift.Swift]

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        self.cmake_options.define('BUILD_SHARED_LIBS', 'YES')

        # Use empty CMake install prefix, since the `DESTDIR` env var is set by
        # `install_with_cmake` later which already has the same prefix.
        self.cmake_options.define('CMAKE_INSTALL_PREFIX', '')

        self.cmake_options.define('CMAKE_BUILD_TYPE', self.args.build_variant)

        build_root = os.path.dirname(self.build_dir)
        swift_build_dir = os.path.join(
            '..', build_root, '%s-%s' % ('swift', host_target))
        swift_cmake_dir = os.path.join(swift_build_dir, 'cmake', 'modules')
        self.cmake_options.define('SwiftSyntax_DIR:PATH', swift_cmake_dir)

        self.build_with_cmake([], self.args.build_variant, [],
                              prefer_native_toolchain=True)

    def should_test(self, host_target):
        # TODO
        return False

    def should_install(self, host_target):
        return self.args.install_swift_testing

    def install(self, host_target):
        install_destdir = self.host_install_destdir(host_target)
        install_prefix = install_destdir + self.args.install_prefix

        self.install_with_cmake(['install'], install_prefix)
