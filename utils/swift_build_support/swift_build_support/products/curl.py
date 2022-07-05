# swift_build_support/products/curl.py ---------------------------------------
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
from . import zlib


class LibCurl(cmake_product.CMakeProduct):
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
    def product_source_name(cls):
        """product_source_name -> Bool

        returns 'curl'
        """
        return "curl"

    @classmethod
    def get_dependencies(cls):
        return [zlib.Zlib]

    def should_build(self, host_target):
        """should_build() -> Bool

        Return True if curl should be built
        """
        return self.args.build_curl

    def should_test(self, host_target):
        """should_test() -> Bool

        Returns True if curl should be tested.
        Currently is set to false
        """
        return False

    def should_install(self, host_target):
        """should_install() -> Bool

        Returns True
        If we're building curl, you're going to need it
        """
        return self.args.build_curl

    def install(self, host_target):
        """
        Install curl to the target location
        """
        path = self.host_install_destdir(host_target)
        self.install_with_cmake(['install'], path)

    def build(self, host_target):
        self.cmake_options.define('BUILD_SHARED_LIBS', 'NO')
        self.cmake_options.define('CMAKE_POSITION_INDEPENDENT_CODE', 'YES')

        if self.args.curl_build_variant is None:
            self.args.curl_build_variant = 'Release'
        self.cmake_options.define('CMAKE_BUILD_TYPE:STRING',
                                  self.args.curl_build_variant)
        self.cmake_options.define('CMAKE_INSTALL_LIBDIR', 'lib')
        self.cmake_options.define('CMAKE_INSTALL_PREFIX', '/usr')
        self.cmake_options.define('BUILD_CURL_EXE', 'NO')
        self.cmake_options.define('CMAKE_USE_OPENSSL', 'NO')
        self.cmake_options.define('CURL_CA_PATH', 'none')
        self.cmake_options.define('CMAKE_USE_SCHANNEL', 'NO')
        self.cmake_options.define('CMAKE_USE_LIBSSH2', 'NO')
        self.cmake_options.define('HAVE_POLL_FINE', 'NO')
        self.cmake_options.define('CURL_DISABLE_LDAP', 'YES')
        self.cmake_options.define('CURL_DISABLE_LDAPS', 'YES')
        self.cmake_options.define('CURL_DISABLE_TELNET', 'YES')
        self.cmake_options.define('CURL_DISABLE_DICT', 'YES')
        self.cmake_options.define('CURL_DISABLE_FILE', 'YES')
        self.cmake_options.define('CURL_DISABLE_TFTP', 'YES')
        self.cmake_options.define('CURL_DISABLE_RTSP', 'YES')
        self.cmake_options.define('CURL_DISABLE_PROXY', 'YES')
        self.cmake_options.define('CURL_DISABLE_POP3', 'YES')
        self.cmake_options.define('CURL_DISABLE_IMAP', 'YES')
        self.cmake_options.define('CURL_DISABLE_SMTP', 'YES')
        self.cmake_options.define('CURL_DISABLE_GOPHER', 'YES')
        self.cmake_options.define('CURL_ZLIB', 'YES')
        self.cmake_options.define('ENABLE_UNIX_SOCKETS', 'NO')
        self.cmake_options.define('ENABLE_THREADED_RESOLVER', 'NO')

        (platform, arch) = host_target.split('-')
        common_c_flags = ' '.join(self.common_cross_c_flags(platform, arch))
        self.cmake_options.define('CMAKE_C_FLAGS', common_c_flags)
        self.cmake_options.define('CMAKE_CXX_FLAGS', common_c_flags)

        if host_target.startswith("macosx") or \
           host_target.startswith("iphone") or \
           host_target.startswith("appletv") or \
           host_target.startswith("watch"):
            toolchain_file = self.generate_darwin_toolchain_file(platform, arch)
            self.cmake_options.define('CMAKE_TOOLCHAIN_FILE:PATH', toolchain_file)
        elif platform == "linux":
            toolchain_file = self.generate_linux_toolchain_file(platform, arch)
            self.cmake_options.define('CMAKE_TOOLCHAIN_FILE:PATH', toolchain_file)

        if self.args.build_zlib:
            # If we're building zlib, make cmake search in the built toolchain
            toolchain_path = self.host_install_destdir(host_target)
            self.cmake_options.define('CMAKE_FIND_ROOT_PATH', toolchain_path)
        self.build_with_cmake(['libcurl'], self.args.curl_build_variant, [])
