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

from build_swift.build_swift.versions import Version

from . import cmake_product
from . import foundation
from . import product
from . import swift
from . import swift_testing_macros
from .. import shell


class SwiftTesting(product.Product):
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
        return [swift.Swift,
                swift_testing_macros.SwiftTestingMacros,
                foundation.Foundation]

    def should_clean(self, host_target):
        # Workaround for 'swift-testing' not detecting compiler/stdlib changes.
        return True

    def should_build(self, host_target):
        return True

    def should_test(self, host_target):
        # TODO: Implement.
        return False

    def should_install(self, host_target):
        return self.args.install_swift_testing

    def _cmake_product(self, host_target, build_shared_libs):
        build_root = os.path.dirname(self.build_dir)
        product_name = self.product_name()
        if not build_shared_libs and not host_target.startswith('wasi'):
            product_name += '_static'
        build_dir = os.path.join(
            build_root, '%s-%s' % (product_name, host_target))

        product = SwiftTestingCMakeShim(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=self.source_dir,
            build_dir=build_dir)
        product.build_shared_libs = build_shared_libs
        return product

    def _cmake_products(self, host_target):
        if host_target.startswith('wasi'):
            return [self._cmake_product(host_target, build_shared_libs=False)]
        if self.is_darwin_host(host_target):
            return [self._cmake_product(host_target, build_shared_libs=True)]
        return [
            self._cmake_product(host_target, build_shared_libs=True),
            self._cmake_product(host_target, build_shared_libs=False),
        ]

    def _for_each_host_target(self, base_target, body):
        body(base_target)

        # For Darwin host, 'build' is only called for the builder.
        # Manually iterate the cross compile hosts.
        if self.has_cross_compile_hosts() and self.is_darwin_host(base_target):
            for target in self.args.cross_compile_hosts:
                body(target)

    def _clean_with_cmake(self, host_target):
        for product in self._cmake_products(host_target):
            product.clean(host_target)

    def clean(self, host_target):
        self._for_each_host_target(host_target, self._clean_with_cmake)

    def _build_with_cmake(self, host_target):
        for product in self._cmake_products(host_target):
            product.build(host_target)

    def build(self, host_target):
        self._for_each_host_target(host_target, self._build_with_cmake)

    def _install_with_cmake(self, host_target):
        for product in self._cmake_products(host_target):
            product.install(host_target)

    def install(self, host_target):
        self._for_each_host_target(host_target, self._install_with_cmake)


class SwiftTestingCMakeShim(cmake_product.CMakeProduct):
    def clean(self, host_target):
        shell.rmtree(self.build_dir)

    def build(self, host_target):
        override_deployment_version = None
        if host_target.startswith('macosx'):
            if Version(self.args.darwin_deployment_version_osx) < Version('14.0'):
                override_deployment_version = '14.0'

            # On Darwin platforms, specify a module ABI name suffix so that this
            # copy does not encounter runtime conflicts with a copy loaded from
            # a pre-existing source such as Xcode.
            self.cmake_options.define('SwiftTesting_MODULE_ABI_NAME_SUFFIX', '_toolchain')

        self.cmake_options.define('BUILD_SHARED_LIBS',
                                  'TRUE' if self.build_shared_libs else 'FALSE')

        # Use empty CMake install prefix, since the `DESTDIR` env var is set by
        # `install_with_cmake` later which already has the same prefix.
        self.cmake_options.define('CMAKE_INSTALL_PREFIX', '')

        self.cmake_options.define('CMAKE_BUILD_TYPE', self.args.build_variant)

        self.cmake_options.define('CMAKE_Swift_COMPILATION_MODE', 'wholemodule')

        # FIXME: If we build macros for the builder, specify the path.
        self.cmake_options.define('SwiftTesting_MACRO', 'NO')

        if not self.is_darwin_host(host_target) and \
                not host_target.startswith('wasi'):
            build_root = os.path.dirname(self.build_dir)

            # Use a compiler from the build tree rather than the toolchain being
            # assembled. The latter may already contain shared corelibs module
            # maps, which conflict with the build-tree dependencies when
            # building the static variant.
            native_swift_tools_path = self.args.native_swift_tools_path or \
                os.path.join(
                    build_root, 'swift-%s' % self.args.host_target, 'bin')
            self.cmake_options.define(
                'CMAKE_Swift_COMPILER:PATH',
                os.path.join(native_swift_tools_path, 'swiftc'))

            dependency_suffix = '' if self.build_shared_libs else '_static'
            dispatch_build_dir = os.path.join(
                build_root, 'libdispatch%s-%s' %
                (dependency_suffix, host_target))
            foundation_build_dir = os.path.join(
                build_root, 'foundation%s-%s' %
                (dependency_suffix, host_target))
            self.cmake_options.define(
                'dispatch_DIR:PATH',
                os.path.join(dispatch_build_dir, 'cmake', 'modules'))
            self.cmake_options.define(
                'Foundation_DIR:PATH',
                os.path.join(foundation_build_dir, 'cmake', 'modules'))

        self.generate_toolchain_file_for_darwin_or_linux(
            host_target, override_macos_deployment_version=override_deployment_version)
        self.build_with_cmake([], self.args.build_variant, [],
                              prefer_native_toolchain=True)

    def install(self, host_target):
        install_destdir = self.host_install_destdir(host_target)
        install_prefix = install_destdir + self.args.install_prefix

        self.install_with_cmake(['install'], install_prefix)

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False
