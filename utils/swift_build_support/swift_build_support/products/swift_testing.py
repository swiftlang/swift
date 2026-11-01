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

from ..host_specific_configuration \
    import HostSpecificConfiguration


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
        return not self.is_cross_compile_target(host_target)

    def should_build(self, host_target):
        return not self.is_cross_compile_target(host_target)

    def should_test(self, host_target):
        # TODO: Implement.
        return False

    def should_install(self, host_target):
        return self.args.install_swift_testing and not self.is_cross_compile_target(host_target)

    def _cmake_product(self, stdlib_target):
        build_root = os.path.dirname(self.build_dir)
        build_dir = os.path.join(
            build_root, '%s-%s' % (self.product_name(), stdlib_target))

        return SwiftTestingCMakeShim(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=self.source_dir,
            build_dir=build_dir)

    def _for_each_swifttesting_target(self, base_target, body):
        body(base_target)

        # For Darwin host, 'build' is only called for the builder.
        # Manually iterate the macos targets
        if self.is_darwin_host(base_target):
            config = HostSpecificConfiguration(base_target, self.args)
            stdlib_build_targets = [
                build_target.removeprefix("swift-stdlib-").removeprefix("swift-test-stdlib-")
                for build_target in config.swift_stdlib_build_targets
            ]
            macos_targets = [
                target for target in stdlib_build_targets
                if target.startswith("macos") and target != base_target
            ]
            for macos_target in macos_targets:
                body(macos_target)

    def _clean_with_cmake(self, host_target):
        self._cmake_product(host_target).clean(host_target)

    def clean(self, host_target):
        self._for_each_swifttesting_target(host_target, self._clean_with_cmake)

    def _build_with_cmake(self, host_target):
        self._cmake_product(host_target).build(host_target)

    def build(self, host_target):
        self._for_each_swifttesting_target(host_target, self._build_with_cmake)

    def install(self, host_target):
        intermediate_destdirs = []

        def _install_with_cmake(target):
            intermediate_destdirs.append(self._cmake_product(target).install(target))

        self._for_each_swifttesting_target(host_target, _install_with_cmake)

        destdir = self.host_install_destdir(host_target)
        # Assume all targets generates the same files
        first_intermediate_destdir = intermediate_destdirs[0]
        for root, dirs, files in os.walk(first_intermediate_destdir):
            rel_path = os.path.relpath(root, first_intermediate_destdir)
            dest_root = os.path.join(destdir, rel_path)

            if not os.path.exists(dest_root):
                shell.makedirs(dest_root)

            for f in files:
                file_rel_path = os.path.join(rel_path, f)
                dest_file = os.path.join(destdir, file_rel_path)
                src_file = os.path.join(first_intermediate_destdir, file_rel_path)

                # Check if this is a binary that needs lipo
                if len(intermediate_destdirs) > 1 and (f.endswith('.dylib') or f.endswith('.a')):
                    lipo_cmd = ['lipo', '-create', '-output', dest_file]
                    for intermediate_destdir in intermediate_destdirs:
                        lipo_cmd.append(os.path.join(intermediate_destdir, file_rel_path))
                    shell.call(lipo_cmd)
                else:
                    shell.copy(src_file, dest_file)

            # For directories, we mostly care about .swiftmodule directories
            for d in list(dirs):
                if d.endswith('.swiftmodule'):
                    module_rel_path = os.path.join(rel_path, d)
                    dest_module_dir = os.path.join(destdir, module_rel_path)
                    if not os.path.exists(dest_module_dir):
                        shell.makedirs(dest_module_dir)

                    # Copy all contents from all targets into this directory
                    for intermediate_destdir in intermediate_destdirs:
                        src_module_dir = os.path.join(intermediate_destdir, module_rel_path)
                        if os.path.exists(src_module_dir):
                            for item in os.listdir(src_module_dir):
                                shell.copy(os.path.join(src_module_dir, item),
                                           os.path.join(dest_module_dir, item))
                    # We don't want os.walk to descend into .swiftmodule because we handled it
                    dirs.remove(d)


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

    def install(self, host_target):
        install_destdir = os.path.join(
            os.path.dirname(self.build_dir), f'swifttesting-{host_target}-install')
        shell.rmtree(install_destdir)
        install_prefix = install_destdir + self.args.install_prefix

        self.install_with_cmake(['install'], install_prefix)

        return install_destdir

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False
