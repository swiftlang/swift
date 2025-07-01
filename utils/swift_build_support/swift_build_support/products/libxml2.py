# swift_build_support/products/libxml2.py ------------------------------------
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

from . import cmake_product
from . import earlyswiftdriver


class LibXML2(cmake_product.CMakeProduct):
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

        Return True if libxml2 should be built
        """
        return self.args.build_libxml2

    def should_test(self, host_target):
        """should_test() -> Bool

        Returns True if libxml2 should be tested.
        Currently is set to false
        """
        return False

    def should_install(self, host_target):
        """should_install() -> Bool

        Returns True if we're building libxml2
        """
        return self.args.build_libxml2

    def install(self, host_target):
        """
        Install libxml2 to the target location
        """
        path = self.host_install_destdir(host_target)
        self.install_with_cmake(['install'], path)

    async def build(self, host_target):
        self.cmake_options.define('BUILD_SHARED_LIBS', 'NO')
        self.cmake_options.define('CMAKE_POSITION_INDEPENDENT_CODE', 'YES')

        if self.args.libxml2_build_variant is None:
            self.args.libxml2_build_variant = "Release"
        self.cmake_options.define('CMAKE_BUILD_TYPE:STRING',
                                  self.args.libxml2_build_variant)
        self.cmake_options.define('CMAKE_INSTALL_PREFIX', '/usr')
        self.cmake_options.define('LIBXML2_WITH_THREADS', 'YES')
        self.cmake_options.define('LIBXML2_WITH_ICONV', 'NO')
        self.cmake_options.define('LIBXML2_WITH_ICU', 'NO')
        self.cmake_options.define('LIBXML2_WITH_LZMA', 'NO')
        self.cmake_options.define('LIBXML2_WITH_PYTHON', 'NO')
        self.cmake_options.define('LIBXML2_WITH_TESTS', 'NO')
        self.cmake_options.define('LIBXML2_WITH_ZLIB', 'NO')

        self.generate_toolchain_file_for_darwin_or_linux(host_target)
        self.build_with_cmake(["LibXml2"], self.args.libxml2_build_variant, [])
