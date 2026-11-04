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
from ..targets import StdlibDeploymentTarget


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

    def _cmake_product(self, host_target):
        build_root = os.path.dirname(self.build_dir)
        build_dir = os.path.join(
            build_root, '%s-%s' % (self.product_name(), host_target))

        return SwiftTestingCMakeShim(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=self.source_dir,
            build_dir=build_dir)

    def _infer_macos_targets(self, base_target):
        macos_targets = []

        # On Darwin we need to ensure to build for all supported macOS architectures
        # if those are not crosscompiled, ensure we build and install those as part of
        # the host build
        if self.is_darwin_host(base_target) and base_target == self.args.host_target:
            cross_compile_targets = self.args.cross_compile_hosts if self.has_cross_compile_hosts() else [] 
            for macos_target in StdlibDeploymentTarget.OSX.targets:
                if (macos_target.name not in cross_compile_targets + [ base_target ]
                        and (
                            not self.args.swift_darwin_supported_archs
                            or macos_target.arch in self.args.swift_darwin_supported_archs)):
                    macos_targets.append(macos_target.name)

        return macos_targets

    def _for_each_host_and_macos_target(self, base_target, body):
        body(base_target)

        for target in self._infer_macos_targets(base_target):
            body(target)

    def _clean_with_cmake(self, host_target):
        self._cmake_product(host_target).clean(host_target)

    def clean(self, host_target):
        self._for_each_host_and_macos_target(host_target, self._clean_with_cmake)

    def _build_with_cmake(self, host_target):
        self._cmake_product(host_target).build(host_target)

    def build(self, host_target):
        self._for_each_host_and_macos_target(host_target, self._build_with_cmake)

    def _install_with_cmake(self, host_target):
        self._cmake_product(host_target).install(host_target)

    def install(self, host_target):
        macos_targets = self._infer_macos_targets(host_target)

        if macos_targets:
            final_install_destdir = self.host_install_destdir(host_target)
            intermediate_destdirs = []
            for target in [ host_target ] + macos_targets:
                intermediate_destdir = os.path.join(
                        os.path.dirname(self.build_dir), f'swifttesting-{target}-install')
                self._cmake_product(target).install(target, intermediate_destdir)
                intermediate_destdirs.append(intermediate_destdir)

            shell.call(
                [
                    os.path.join(self.source_dir, "../swift", "utils/recursive-lipo"),
                    "-v" if self.args.verbose_build else "",
                    "--destination", os.path.join(final_install_destdir)
                ] + intermediate_destdirs)
        else:
            self._cmake_product(host_target).install(host_target)


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

        build_shared_libs = not host_target.startswith('wasi')
        self.cmake_options.define('BUILD_SHARED_LIBS',
                                  'TRUE' if build_shared_libs else 'FALSE')

        # Use empty CMake install prefix, since the `DESTDIR` env var is set by
        # `install_with_cmake` later which already has the same prefix.
        self.cmake_options.define('CMAKE_INSTALL_PREFIX', '')

        self.cmake_options.define('CMAKE_BUILD_TYPE', self.args.build_variant)

        self.cmake_options.define('CMAKE_Swift_COMPILATION_MODE', 'wholemodule')

        # FIXME: If we build macros for the builder, specify the path.
        self.cmake_options.define('SwiftTesting_MACRO', 'NO')

        self.generate_toolchain_file_for_darwin_or_linux(
            host_target, override_macos_deployment_version=override_deployment_version)
        self.build_with_cmake([], self.args.build_variant, [],
                              prefer_native_toolchain=True)

    def install(self, host_target, override_install_destdir=None):
        install_destdir = override_install_destdir if override_install_destdir else self.host_install_destdir(host_target)
        install_prefix = install_destdir + self.args.install_prefix

        self.install_with_cmake(['install'], install_prefix)

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False
