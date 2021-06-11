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

from build_swift.build_swift.wrappers import xcrun

from . import cmake_product
from . import earlyswiftdriver


class CMark(cmake_product.CMakeProduct):
    @classmethod
    def is_build_script_impl_product(cls):
        """is_build_script_impl_product -> bool

        Whether this product is produced by build-script-impl.
        """
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        """is_before_build_script_impl_product -> bool

        Whether this product is build before any build-script-impl products.
        """
        return True

    # EarlySwiftDriver is the root of the graph, and is the only dependency of
    # this product.
    @classmethod
    def get_dependencies(cls):
        return [earlyswiftdriver.EarlySwiftDriver]

    def should_build(self, host_target):
        """should_build() -> Bool

        Whether or not this product should be built with the given arguments.
        """
        return self.args.build_cmark

    def build(self, host_target):
        """build() -> void

        Perform the build, for a non-build-script-impl product.
        """
        self.cmake_options.define('CMAKE_BUILD_TYPE:STRING',
                                  self.args.cmark_build_variant)

        (platform, arch) = host_target.split('-')

        if host_target.startswith("macosx") or \
           host_target.startswith("iphone") or \
           host_target.startswith("appletv") or \
           host_target.startswith("watch"):

            cmake_os_sysroot = xcrun.sdk_path(platform)

            cmake_osx_deployment_target = ''
            if platform == "macosx":
                cmake_osx_deployment_target = self.args.darwin_deployment_version_osx

            common_c_flags = ' '.join(self.common_cross_c_flags(platform, arch))

            self.cmake_options.define('CMAKE_C_FLAGS', common_c_flags)
            self.cmake_options.define('CMAKE_CXX_FLAGS', common_c_flags)
            self.cmake_options.define('CMAKE_OSX_SYSROOT:PATH', cmake_os_sysroot)
            self.cmake_options.define('CMAKE_OSX_DEPLOYMENT_TARGET',
                                      cmake_osx_deployment_target)
            self.cmake_options.define('CMAKE_OSX_ARCHITECTURES', arch)

        self.build_with_cmake(["all"], self.args.cmark_build_variant, [])

    def should_test(self, host_target):
        """should_test() -> Bool

        Whether or not this product should be tested with the given arguments.
        """
        if self.is_cross_compile_target(host_target):
            return False

        return self.args.test_cmark

    def test(self, host_target):
        """
        Perform the test phase for the product.

        This phase might build and execute the product tests.
        """
        executable_target = 'api_test'
        results_targets = ['test']
        if self.args.cmake_generator == 'Xcode':
            # Xcode generator uses "RUN_TESTS" instead of "test".
            results_targets = ['RUN_TESTS']

        self.test_with_cmake(executable_target, results_targets,
                             self.args.cmark_build_variant, [])

    def should_install(self, host_target):
        """should_install() -> Bool

        Whether or not this product should be installed with the given
        arguments.
        """
        return self.args.install_all

    def install(self, host_target):
        """
        Perform the install phase for the product.

        This phase might copy the artifacts from the previous phases into a
        destination directory.
        """
        self.install_with_cmake(["install"], self.host_install_destdir(host_target))
