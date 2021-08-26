# swift_build_support/products/backdeployconcurrency.py ---------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os
import platform

from . import cmake_product
from . import cmark
from . import libcxx
from . import libicu
from . import llvm
from . import swift


class BackDeployConcurrency(cmake_product.CMakeProduct):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return os.path.join("swift")

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return platform.system() == 'Darwin'

    def build(self, host_target):
        build_variant = 'RelWithDebInfo'
        self.cmake_options.define('CMAKE_BUILD_TYPE:STRING', build_variant)

        (platform, arch) = host_target.split('-')

        common_c_flags = ' '.join(self.common_cross_c_flags(platform, arch))
        self.cmake_options.define('CMAKE_C_FLAGS', common_c_flags)
        self.cmake_options.define('CMAKE_CXX_FLAGS', common_c_flags)

#        toolchain_file = self.generate_darwin_toolchain_file(platform, arch)
#        self.cmake_options.define('CMAKE_TOOLCHAIN_FILE:PATH', toolchain_file)
        self.cmake_options.define(
            'TOOLCHAIN_DIR:PATH',
            self.install_toolchain_path(host_target))
        self.cmake_options.define(
            'SWIFT_NATIVE_SWIFT_TOOLS_PATH:PATH',
            os.path.join(self.install_toolchain_path(host_target), 'bin'))

        self.cmake_options.define('SWIFT_EMBED_BITCODE_SECTION:BOOL', True)
        self.cmake_options.define('SWIFT_ENABLE_MACCATALYST:BOOL', True)
        self.cmake_options.define('CMAKE_CROSSCOMPILING:BOOL', True)

        # Only build the back-deployment concurrency library, nothing else
        self.cmake_options.define(
            'BUILD_SWIFT_CONCURRENCY_BACK_DEPLOYMENT_LIBRARIES:BOOL', True)
        self.cmake_options.define('SWIFT_INCLUDE_TOOLS:BOOL', False)
        self.cmake_options.define(
            'SWIFT_BUILD_STDLIB_EXTRA_TOOLCHAIN_CONTENT:BOOL', False)
        self.cmake_options.define(
            'SWIFT_BUILD_TEST_SUPPORT_MODULES:BOOL', False)
        self.cmake_options.define('SWIFT_BUILD_STDLIB:BOOL', False)
        self.cmake_options.define('SWIFT_BUILD_DYNAMIC_STDLIB:BOOL', False)
        self.cmake_options.define('SWIFT_BUILD_STATIC_STDLIB:BOOL', False)
        self.cmake_options.define('SWIFT_BUILD_REMOTE_MIRROR:BOOL', False)
        self.cmake_options.define('SWIFT_BUILD_SDK_OVERLAY:BOOL', False)
        self.cmake_options.define('SWIFT_BUILD_DYNAMIC_SDK_OVERLAY:BOOL', False)
        self.cmake_options.define('SWIFT_BUILD_STATIC_SDK_OVERLAY:BOOL', False)

        self.cmake_options.define('SWIFT_HOST_VARIANT_ARCH:STRING', arch)
        self.cmake_options.define('BUILD_STANDALONE:BOOL', True)

        # Only install the "stdlib" component, which contains the concurrency
        # module.
        self.cmake_options.define('SWIFT_INSTALL_COMPONENTS:STRING', 'back-deployment')

        # Figure out the SDKs to build.
        # NOTE: This normally happens down in build-script-impl, so we have
        # to re-implement the logic here.
        sdks_to_build = ['OSX']
        if self.args.build_ios_device:
            sdks_to_build.append('IOS')
        if self.args.build_ios_simulator:
            sdks_to_build.append('IOS_SIMULATOR')
        if self.args.build_tvos_device:
            sdks_to_build.append('TVOS')
        if self.args.build_tvos_simulator:
            sdks_to_build.append('TVOS_SIMULATOR')
        if self.args.build_watchos_device:
            sdks_to_build.append('WATCHOS')
        if self.args.build_watchos_simulator:
            sdks_to_build.append('WATCHOS_SIMULATOR')
        self.cmake_options.define('SWIFT_SDKS:STRING', ';'.join(sdks_to_build))

        build_root = os.path.dirname(self.build_dir)
        llvm_build_dir = os.path.join(
            '..', build_root, '%s-%s' % ('llvm', host_target))
        llvm_cmake_dir = os.path.join(llvm_build_dir, 'lib', 'cmake', 'llvm')
        self.cmake_options.define('LLVM_DIR:PATH', llvm_cmake_dir)

        dest_dir = self.host_install_destdir(host_target)
        self.cmake_options.define('SWIFT_DEST_ROOT:PATH=', dest_dir)
        self.cmake_options.define('SWIFT_HOST_VARIANT_SDK:STRING', 'OSX')
        self.cmake_options.define('CMAKE_INSTALL_PREFIX', "")

        # Configure back-deployment targets
        self.cmake_options.define(
            'SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX:STRING', '10.15')
        self.cmake_options.define(
            'SWIFT_DARWIN_DEPLOYMENT_VERSION_IOS:STRING', '13.0')
        self.cmake_options.define(
            'SWIFT_DARWIN_DEPLOYMENT_VERSION_MACCATALYST:STRING', '13.0')
        self.cmake_options.define(
            'SWIFT_DARWIN_DEPLOYMENT_VERSION_TVOS:STRING', '13.0')
        self.cmake_options.define(
            'SWIFT_DARWIN_DEPLOYMENT_VERSION_WATCHOS:STRING', '6.0')

        self.build_with_cmake(["back-deployment"], build_variant, [])

    def should_test(self, host_target):
        return False

    def test(self, host_target):
        raise RuntimeError("Testing not implemented")

    def should_install(self, host_target):
        return False

    def install(self, host_target):
        destdir = self.host_install_destdir(host_target) + self.args.install_prefix
        self.install_with_cmake(["install-back-deployment"], destdir)

    @classmethod
    def get_dependencies(cls):
        return [cmark.CMark,
                llvm.LLVM,
                libcxx.LibCXX,
                libicu.LibICU,
                swift.Swift]
