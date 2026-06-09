# swift_build_support/products/emscriptenswiftsdk.py ---------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2024-2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

"""
Build an Emscripten Swift SDK bundle.

Currently only the stdlib is packaged.  Foundation, swift-testing, and
XCTest will be added once those libraries have been ported to Emscripten.
Platform-specific CMake options live in ``_append_platform_cmake_options``;
shared build logic is delegated to ``wasmswiftsdkhelpers``.
"""

import os
import shutil

from . import emscriptensysroot
from . import product
from .emscriptenstdlib import EmscriptenStdlib
from ..helpers import wasmswiftsdkhelpers as helpers


class EmscriptenSwiftSDK(product.Product):
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
        return self.args.build_emscriptenstdlib

    def should_test(self, host_target):
        return False

    def _append_platform_cmake_options(self, cmake_options,
                                       swift_host_triple, sysroot,
                                       extra_swift_flags):
        cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'Emscripten')
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

        swift_flags = ['-sdk', sysroot, '-resource-dir',
                       swift_resource_dir] + extra_swift_flags
        c_flags = ['-resource-dir', clang_resource_dir]
        cxx_flags = c_flags + ['-fno-exceptions']

        # Emscripten keeps compatibility headers (e.g. xlocale.h) in a
        # separate include/compat directory that emcc adds automatically.
        # Since we use raw clang, we need to add it ourselves.
        compat_include = os.path.join(sysroot, 'include', 'compat')
        c_flags.extend(['-isystem', compat_include])
        cxx_flags.extend(['-isystem', compat_include])

        cmake_options.define('CMAKE_Swift_FLAGS', ' '.join(swift_flags))
        cmake_options.define('CMAKE_C_FLAGS', ' '.join(c_flags))
        cmake_options.define('CMAKE_CXX_FLAGS', ' '.join(cxx_flags))
        cmake_options.define('CMAKE_Swift_COMPILER_FORCED', 'TRUE')
        cmake_options.define('CMAKE_CXX_COMPILER_FORCED', 'TRUE')
        # Emscripten's crt1.o requires __main_void and exit, so CMake's
        # try_compile linking tests fail.  Build static libraries instead
        # of executables for compiler/feature detection so that both the
        # initial compiler test and check_include_files() work correctly.
        cmake_options.define('CMAKE_TRY_COMPILE_TARGET_TYPE', 'STATIC_LIBRARY')
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

        swift_host_triple = 'wasm32-unknown-emscripten'
        clang_multiarch_triple = 'wasm32-emscripten'

        stdlib_build_path = os.path.join(
            build_root, '%s-%s' % ('emscriptenstdlib', host_target))
        sysroot = emscriptensysroot.EmscriptenSysroot.sysroot_install_path(
            build_root, clang_multiarch_triple)
        resource_dir = \
            emscriptensysroot.EmscriptenSysroot.resource_dir_install_path(
                build_root, clang_multiarch_triple)

        dest_dir = helpers.target_package_path(
            self.build_dir, swift_host_triple)
        helpers.install_stdlib_and_resources(
            self.toolchain.cmake, stdlib_build_path,
            resource_dir, dest_dir)

        # TODO: Build XCTest for Emscripten once it has been ported.
        # The platform CMake options are already wired up via
        # _append_platform_cmake_options; the helper call below just needs
        # to be uncommented when the porting work is done.

        def append_cmake_opts(cmake_options, extra_swift_flags,
                              _triple=swift_host_triple,
                              _sysroot=sysroot):
            self._append_platform_cmake_options(
                cmake_options, _triple, _sysroot, extra_swift_flags)

        host_toolchain_path = self.native_toolchain_path(
            self.args.host_target)

        # Clean sub-build directories when reconfiguring so that stale
        # CMake cache entries (e.g. HAVE_UNISTD_H) don't persist.
        if self.args.reconfigure:
            for subdir in ['libxml2', 'foundation']:
                d = os.path.join(self.build_dir, subdir)
                if os.path.isdir(d):
                    shutil.rmtree(d)

        # helpers.build_libxml2(
        #    self.args, self.toolchain, self.source_dir, self.build_dir,
        #    swift_host_triple, clang_multiarch_triple,
        #    False, sysroot, append_cmake_opts)
        # helpers.build_foundation(
        #    self.args, self.toolchain, self.source_dir, self.build_dir,
        #    swift_host_triple, clang_multiarch_triple,
        #    sysroot, dest_dir, host_toolchain_path, append_cmake_opts)
        # helpers.build_swift_testing(
        #    self.args, self.toolchain, self.source_dir, self.build_dir,
        #    swift_host_triple, dest_dir, append_cmake_opts)
        # helpers.build_xctest(
        #     self.args, self.toolchain, self.source_dir, self.build_dir,
        #     swift_host_triple, dest_dir, append_cmake_opts)

        swift_version = os.environ.get('TOOLCHAIN_VERSION',
                                       'swift-DEVELOPMENT-SNAPSHOT')

        swift_run = helpers.find_swift_run(
            self.args, self.toolchain, host_target,
            self.install_toolchain_path(host_target))
        # Append the Emscripten Swift SDK to the shared wasm
        # `.artifactbundle`. The bundle is reused across wasi /
        # wasi-threads / emscripten via `--incremental` +
        # `--bundle-name canonical_bundle_name()`.
        helpers.generate_swift_sdk(
            swift_run=swift_run,
            source_dir=self.source_dir,
            build_dir=self.build_dir,
            triple=swift_host_triple,
            sysroot=sysroot,
            package_path=dest_dir,
            bundle_name=helpers.canonical_bundle_name(),
            swift_version=swift_version,
        )

    def test(self, host_target):
        pass

    def should_install(self, host_target):
        return False

    @classmethod
    def get_dependencies(cls):
        return [EmscriptenStdlib]
