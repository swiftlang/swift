# swift_build_support/products/wasiswiftsdk.py ------------------*- python -*-
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

from . import product
from . import wasisysroot
from ..helpers import wasmswiftsdkhelpers as helpers
from .wasistdlib import WASIStdlib, WASIThreadsStdlib


class WASISwiftSDK(product.Product):
    @classmethod
    def product_source_name(cls):
        return "swift-sdk-generator"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return self.args.build_wasistdlib

    def should_test(self, host_target):
        return False

    def _append_platform_cmake_options(self, cmake_options,
                                       swift_host_triple, threading_flags,
                                       sysroot, extra_swift_flags):
        cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'WASI')
        cmake_options.define('CMAKE_SYSTEM_PROCESSOR:STRING', 'wasm32')
        cmake_options.define('CMAKE_C_COMPILER_TARGET', swift_host_triple)
        cmake_options.define('CMAKE_CXX_COMPILER_TARGET', swift_host_triple)
        cmake_options.define(
            'CMAKE_Swift_COMPILER_TARGET', swift_host_triple)
        cmake_options.define('CMAKE_SYSROOT', sysroot)

        dest_dir = helpers.target_package_path(
            self.build_dir, swift_host_triple)
        swift_resource_dir = os.path.join(
            dest_dir, 'usr', 'lib', 'swift_static')
        clang_resource_dir = os.path.join(swift_resource_dir, 'clang')

        threading_swift_flags, threading_clang_flags = threading_flags
        swift_flags = ['-sdk', sysroot, '-resource-dir',
                       swift_resource_dir] + extra_swift_flags
        swift_flags.extend(threading_swift_flags)
        c_flags = ['-resource-dir', clang_resource_dir]
        cxx_flags = c_flags + ['-fno-exceptions']
        c_flags.extend(threading_clang_flags)
        cxx_flags.extend(threading_clang_flags)

        cmake_options.define('CMAKE_Swift_FLAGS', ' '.join(swift_flags))
        cmake_options.define('CMAKE_C_FLAGS', ' '.join(c_flags))
        cmake_options.define('CMAKE_CXX_FLAGS', ' '.join(cxx_flags))
        cmake_options.define('CMAKE_Swift_COMPILER_FORCED', 'TRUE')
        cmake_options.define('CMAKE_CXX_COMPILER_FORCED', 'TRUE')
        cmake_options.define('CMAKE_BUILD_TYPE', self.args.build_variant)

        if not self.args.build_runtime_with_host_compiler:
            native_toolchain_path = self.native_toolchain_path(
                self.args.host_target)
            cmake_options.define('CMAKE_AR', os.path.join(
                native_toolchain_path, 'bin', 'llvm-ar'))
            cmake_options.define('CMAKE_RANLIB', os.path.join(
                native_toolchain_path, 'bin', 'llvm-ranlib'))

    def build(self, host_target):
        build_root = os.path.dirname(self.build_dir)

        swift_version = os.environ.get('TOOLCHAIN_VERSION',
                                       'swift-DEVELOPMENT-SNAPSHOT')
        swift_run = helpers.find_swift_run(
            self.args, self.toolchain, host_target,
            self.install_toolchain_path(host_target))

        for stdlib_product, has_pthread in [
            (WASIStdlib, False),
            (WASIThreadsStdlib, True),
        ]:
            swift_host_triple = stdlib_product.swift_triple()
            clang_multiarch_triple = stdlib_product.multiarch_triple()
            stdlib_build_path = os.path.join(
                build_root,
                '%s-%s' % (stdlib_product.product_name(), host_target))
            sysroot = wasisysroot.WASISysroot.sysroot_install_path(
                build_root, clang_multiarch_triple)
            resource_dir = wasisysroot.WASISysroot.resource_dir_install_path(
                build_root, clang_multiarch_triple)

            dest_dir = helpers.target_package_path(
                self.build_dir, swift_host_triple)
            helpers.install_stdlib_and_resources(
                self.toolchain.cmake, stdlib_build_path,
                resource_dir, dest_dir)

            threading_flags = stdlib_product.threading_compile_flags()

            # Create a closure capturing WASI-specific platform config.
            def append_cmake_opts(cmake_options, extra_swift_flags,
                                  _triple=swift_host_triple,
                                  _flags=threading_flags,
                                  _sysroot=sysroot):
                self._append_platform_cmake_options(
                    cmake_options, _triple, _flags,
                    _sysroot, extra_swift_flags)

            host_toolchain_path = self.native_toolchain_path(
                self.args.host_target)

            helpers.build_libxml2(
                self.args, self.toolchain, self.source_dir, self.build_dir,
                swift_host_triple, clang_multiarch_triple,
                has_pthread, sysroot, append_cmake_opts)
            helpers.build_foundation(
                self.args, self.toolchain, self.source_dir, self.build_dir,
                swift_host_triple, clang_multiarch_triple,
                sysroot, dest_dir, host_toolchain_path, append_cmake_opts)
            helpers.build_swift_testing(
                self.args, self.toolchain, self.source_dir, self.build_dir,
                swift_host_triple, dest_dir, append_cmake_opts)
            helpers.build_xctest(
                self.args, self.toolchain, self.source_dir, self.build_dir,
                swift_host_triple, dest_dir, append_cmake_opts)

            # Append this triple's Swift SDK to the shared wasm
            # `.artifactbundle`. The bundle is reused across wasi /
            # wasi-threads / emscripten via `--incremental` + `--bundle-name
            # canonical_bundle_name(swift_version)`.
            helpers.generate_swift_sdk(
                swift_run=swift_run,
                source_dir=self.source_dir,
                build_dir=self.build_dir,
                triple=swift_host_triple,
                sysroot=sysroot,
                package_path=dest_dir,
                bundle_name=helpers.canonical_bundle_name(swift_version),
                swift_version=swift_version,
            )

    def test(self, host_target):
        pass

    def should_install(self, host_target):
        return False

    @classmethod
    def get_dependencies(cls):
        return [WASIStdlib, WASIThreadsStdlib]
