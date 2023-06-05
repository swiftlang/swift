# swift_build_support/products/zstd.py ------------------------------------
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

from . import cmake_product
from . import earlyswiftdriver


class Zstd(cmake_product.CMakeProduct):
    def __init__(self, args, toolchain, source_dir, build_dir):
        # We need to modify the source dir because the CMakeLists.txt is in
        # a subdirectory
        super().__init__(args, toolchain,
                         os.path.join(source_dir, 'build', 'cmake'),
                         build_dir)

    @classmethod
    def is_build_script_impl_product(cls):
        """is_build_script_impl_product -> bool

        Whether this product is produced by build-script-impl
        """
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        """is_before_build_script_impl_product -> bool

        Whether this product is built before any build-script-impl products
        """
        return True

    @classmethod
    def is_nondarwin_only_build_product(cls):
        return True

    @classmethod
    def get_dependencies(cls):
        return [earlyswiftdriver.EarlySwiftDriver]

    def should_build(self, host_target):
        """should_build() -> Bool

        Return True if zstd should be built
        """
        return self.args.build_zstd

    def should_test(self, host_target):
        """should_test() -> Bool

        Returns True if zstd should be tested.
        Currently is set to false
        """
        return False

    def should_install(self, host_target):
        """should_install() -> Bool

        Returns True
        If we're building zstd, you're going to need it
        """
        return self.args.build_zstd

    def install(self, host_target):
        """
        Install zstd to the target location
        """
        path = self.host_install_destdir(host_target)
        self.install_with_cmake(['install'], path)

    def build(self, host_target):
        self.cmake_options.define('ZSTD_BUILD_SHARED', 'NO')
        self.cmake_options.define('ZSTD_BUILD_STATIC', 'YES')
        self.cmake_options.define('ZSTD_BUILD_PROGRAMS', 'NO')
        self.cmake_options.define('CMAKE_POSITION_INDEPENDENT_CODE', 'YES')

        if self.args.zstd_build_variant is None:
            self.args.zstd_build_variant = "Release"
        self.cmake_options.define('CMAKE_BUILD_TYPE:STRING',
                                  self.args.zstd_build_variant)
        self.cmake_options.define('CMAKE_BUILD_TYPE', 'RELEASE')
        self.cmake_options.define('CMAKE_INSTALL_PREFIX', '/usr')

        self.generate_toolchain_file_for_darwin_or_linux(host_target)
        self.build_with_cmake(["all"], self.args.zstd_build_variant, [])
