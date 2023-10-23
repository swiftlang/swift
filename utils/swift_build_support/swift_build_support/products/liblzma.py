# swift_build_support/products/liblzma.py ------------------------------------
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


class Liblzma(cmake_product.CMakeProduct):
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

        Return True if liblzma should be built
        """
        return self.args.build_liblzma

    def should_test(self, host_target):
        """should_test() -> Bool

        Returns True if liblzma should be tested.
        Currently is set to false
        """
        return False

    def should_install(self, host_target):
        """should_install() -> Bool

        Returns True
        If we're building liblzma, you're going to need it
        """
        return self.args.build_liblzma

    def install(self, host_target):
        """
        Install liblzma to the target location
        """
        path = self.host_install_destdir(host_target)
        self.install_with_cmake(['install'], path)

        # Remove the binaries (we don't want them and we can't turn them off)
        try:
            os.unlink(os.path.join(path, 'usr', 'bin', 'xz'))
            os.unlink(os.path.join(path, 'usr', 'bin', 'xzdec'))
        except FileNotFoundError:
            pass

        try:
            os.removedirs(os.path.join(path, 'usr', 'bin'))
        except (FileNotFoundError, OSError):
            pass

        # Also remove the man pages
        try:
            os.unlink(os.path.join(path, 'usr', 'share', 'man1', 'xz.1'))
            os.unlink(os.path.join(path, 'usr', 'share', 'man1', 'xzdec.1'))
        except FileNotFoundError:
            pass

        try:
            os.removedirs(os.path.join(path, 'usr', 'share', 'man1'))
        except (FileNotFoundError, OSError):
            pass

    def build(self, host_target):
        self.cmake_options.define('BUILD_SHARED_LIBS', 'NO')
        self.cmake_options.define('CREATE_XZ_SYMLINKS', 'NO')
        self.cmake_options.define('CREATE_LZMA_SYMLINKS', 'NO')
        self.cmake_options.define('CMAKE_POSITION_INDEPENDENT_CODE', 'YES')

        if self.args.liblzma_build_variant is None:
            self.args.liblzma_build_variant = "Release"
        self.cmake_options.define('CMAKE_BUILD_TYPE:STRING',
                                  self.args.liblzma_build_variant)
        self.cmake_options.define('CMAKE_BUILD_TYPE', 'RELEASE')
        self.cmake_options.define('SKIP_INSTALL_FILES', 'YES')
        self.cmake_options.define('CMAKE_INSTALL_PREFIX', '/usr')
        self.cmake_options.define('BUILD_EXECUTABLES', 'NO')

        self.generate_toolchain_file_for_darwin_or_linux(host_target)
        self.build_with_cmake(["liblzma"], self.args.liblzma_build_variant, [])
