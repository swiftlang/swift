# swift_build_support/products/emscriptenhostswift.py ----------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os
import shutil
import sys

from . import emscriptenhostllvm
from . import emscriptensysroot
from . import emscriptenswiftsdk
from . import product
from .. import shell


class EmscriptenHostSwift(product.Product):
    @classmethod
    def product_source_name(cls):
        return "swift"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def get_dependencies(cls):
        return [emscriptenhostllvm.EmscriptenHostLLVM,
                emscriptenswiftsdk.EmscriptenSwiftSDK]

    def should_build(self, host_target):
        return self.args.build_emscripten_host_swift and \
            not self.is_cross_compile_target(host_target)

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return False

    def build(self, host_target):
        emcmake = self._emcmake_path()
        build_root = os.path.dirname(self.build_dir)
        source_root = os.path.dirname(self.source_dir)

        cache_file = self._cache_file_path()
        shim_file = self._lld_shim_path()

        xllvm_lib = os.path.join(
            build_root, 'emscriptenhostllvm-%s' % host_target, 'lib')
        llvm_cmake_dir = os.path.join(xllvm_lib, 'cmake', 'llvm')
        clang_cmake_dir = os.path.join(xllvm_lib, 'cmake', 'clang')

        emstdlib_dir = os.path.join(
            build_root, 'emscriptenstdlib-%s' % host_target)
        emstdlib_resource_dir = os.path.join(emstdlib_dir, 'lib', 'swift')

        # The -sdk sysroot's <xlocale.h> compat headers live under
        # include/compat.
        built_sysroot = \
            emscriptensysroot.EmscriptenSysroot.sysroot_install_path(
                build_root, 'wasm32-emscripten')
        compat_include = os.path.join(built_sysroot, 'include', 'compat')
        sdk_flags = ';'.join([
            '-resource-dir', emstdlib_resource_dir,
            '-sdk', built_sysroot,
            '-Xcc', '-isystem', '-Xcc', compat_include,
        ])

        # The emcc cache sysroot (host-tool C++ headers); a different sysroot
        # from the built -sdk one above. Both must be passed.
        emcc_sysroot = self._emcc_cache_sysroot(emcmake)

        native_swift_bin = self.args.native_swift_tools_path or \
            os.path.join(build_root, 'swift-%s' % host_target, 'bin')
        native_llvm_bin = self.args.native_llvm_tools_path or \
            os.path.join(build_root, 'llvm-%s' % host_target, 'bin')
        native_clang_bin = self.args.native_clang_tools_path or \
            os.path.join(build_root, 'llvm-%s' % host_target, 'bin')
        swiftc = os.path.join(native_swift_bin, 'swiftc')
        llvm_tblgen = os.path.join(native_llvm_bin, 'llvm-tblgen')
        clang_tblgen = os.path.join(native_clang_bin, 'clang-tblgen')

        cmark_source_dir = os.path.join(source_root, 'cmark')

        if not self.args.dry_run:
            self._require_file(
                cache_file, 'EmscriptenHostSwift.cmake cache not found')
            self._require_file(
                shim_file, 'EmscriptenHostSwiftLLDShim.cmake not found')
            self._require_dir(
                llvm_cmake_dir, 'cross-LLVM not found; build it with '
                '--build-emscripten-host-llvm')
            self._require_dir(
                clang_cmake_dir, 'cross-Clang not found; build it with '
                '--build-emscripten-host-llvm')
            self._require_dir(
                emstdlib_resource_dir, 'emscripten target stdlib not found; '
                'build it with --build-emscripten-stdlib')
            self._require_dir(
                built_sysroot, 'emscripten -sdk sysroot not found; build it '
                'with --build-emscripten-stdlib')
            self._require_file(
                swiftc, 'native swiftc not found; build the native Swift '
                'tools or pass --native-swift-tools-path')
            self._require_file(
                llvm_tblgen, 'native llvm-tblgen not found; build native LLVM '
                'or pass --native-llvm-tools-path')
            self._require_file(
                clang_tblgen, 'native clang-tblgen not found; build it or '
                'pass --native-clang-tools-path')
            self._require_dir(
                emcc_sysroot, 'emcc cache sysroot not found; run emcc once to '
                'populate the cache, or pass --emscripten-path')

        # Build cmark first; the frontend configure below reads its config.
        cmark_build_dir = self._build_cmark_wasm(
            host_target, emcmake, cmark_source_dir, build_root)

        configure_cmd = [
            emcmake, self.toolchain.cmake,
            '-G', 'Ninja',
            '-S', self.source_dir,
            '-B', self.build_dir,
            '-C', cache_file,
            '-DCMAKE_PROJECT_TOP_LEVEL_INCLUDES=' + shim_file,
            '-DSWIFT_EMSCRIPTEN_HOST_LLVM_LIB_DIR=' + xllvm_lib,
            '-DCMAKE_BUILD_TYPE=' + self.args.swift_build_variant,
            '-DLLVM_DIR=' + llvm_cmake_dir,
            '-DClang_DIR=' + clang_cmake_dir,
            '-DSWIFT_PATH_TO_SWIFT_SDK=' + emstdlib_dir,
            '-DSWIFT_COMPILER_SOURCES_SDK_FLAGS=' + sdk_flags,
            '-DSWIFT_EMSCRIPTEN_SYSROOT_PATH=' + emcc_sysroot,
            '-DSWIFT_PATH_TO_CMARK_BUILD=' + cmark_build_dir,
            '-DSWIFT_PATH_TO_CMARK_SOURCE=' + cmark_source_dir,
            '-DLLVM_TABLEGEN=' + llvm_tblgen,
            '-DCLANG_TABLEGEN=' + clang_tblgen,
            '-DSWIFT_NATIVE_LLVM_TOOLS_PATH=' + native_llvm_bin,
            '-DSWIFT_NATIVE_CLANG_TOOLS_PATH=' + native_clang_bin,
            '-DSWIFT_NATIVE_SWIFT_TOOLS_PATH=' + native_swift_bin,
        ]

        # A half-finished configure leaves CMakeCache.txt without build.ninja,
        # which wedges every later build; reconfigure if either is missing.
        # EmscriptenHostSwift.cmake is a `-C` initial cache whose
        # `set(... CACHE ...)` is no-FORCE: editing it has no effect on an
        # existing build dir, even with --reconfigure. Remove the dir first.
        if self._needs_configure(self.build_dir):
            shell.makedirs(self.build_dir)
            shell.call(configure_cmd)

        if self.args.skip_build:
            return

        build_args = ['-j', str(self.args.build_jobs)]
        if self.args.verbose_build:
            build_args.append('-v')
        shell.call([self.toolchain.cmake, '--build', self.build_dir, '--']
                   + build_args + ['swift-frontend'])

    def _build_cmark_wasm(self, host_target, emcmake, cmark_source_dir,
                          build_root):
        """Build a wasm cmark and return its build dir. The shared CMark
        product is native-only, so build a private copy rather than editing
        it."""
        cmark_build_dir = os.path.join(
            build_root, 'emscriptenhostswift-cmark-%s' % host_target)

        if not self.args.dry_run:
            self._require_dir(cmark_source_dir, 'cmark source not found')

        configure_cmd = [
            emcmake, self.toolchain.cmake,
            '-G', 'Ninja',
            '-S', cmark_source_dir,
            '-B', cmark_build_dir,
            '-DCMAKE_BUILD_TYPE=' + self.args.swift_build_variant,
            '-DBUILD_SHARED_LIBS=OFF',
            '-DBUILD_TESTING=OFF',
        ]
        if self._needs_configure(cmark_build_dir):
            shell.makedirs(cmark_build_dir)
            shell.call(configure_cmd)

        if not self.args.skip_build:
            build_args = ['-j', str(self.args.build_jobs)]
            if self.args.verbose_build:
                build_args.append('-v')
            shell.call([self.toolchain.cmake, '--build', cmark_build_dir, '--']
                       + build_args)

        return cmark_build_dir

    def _needs_configure(self, build_dir):
        cmake_cache = os.path.join(build_dir, 'CMakeCache.txt')
        build_ninja = os.path.join(build_dir, 'build.ninja')
        return self.args.reconfigure or not os.path.isfile(cmake_cache) \
            or not os.path.isfile(build_ninja)

    def _require_file(self, path, message):
        if not os.path.isfile(path):
            print('error: %s (expected at %s)' % (message, path),
                  file=sys.stderr)
            sys.exit(1)

    def _require_dir(self, path, message):
        if not os.path.isdir(path):
            print('error: %s (expected at %s)' % (message, path),
                  file=sys.stderr)
            sys.exit(1)

    def _emcmake_path(self):
        if self.args.emscripten_path:
            candidate = os.path.join(self.args.emscripten_path, 'emcmake')
            if os.path.isfile(candidate) or self.args.dry_run:
                return candidate
            print('error: emcmake not found at %s (from --emscripten-path)'
                  % candidate, file=sys.stderr)
            sys.exit(1)
        found = shutil.which('emcmake')
        if found is not None:
            return found
        if self.args.dry_run:
            return 'emcmake'
        print('error: `emcmake` not found; pass --emscripten-path pointing at '
              'the Emscripten checkout, or put emcmake on PATH', file=sys.stderr)
        sys.exit(1)

    def _emcc_cache_sysroot(self, emcmake):
        em_dir = self.args.emscripten_path or \
            os.path.dirname(os.path.realpath(emcmake))
        if self.args.dry_run:
            # Avoid invoking em-config under --dry-run; guess <path>/cache.
            return os.path.join(em_dir, 'cache', 'sysroot')
        cache_dir = shell.capture(
            [os.path.join(em_dir, 'em-config'), 'CACHE']).strip()
        return os.path.join(cache_dir, 'sysroot')

    def _cache_file_path(self):
        return os.path.join(self.source_dir, 'cmake', 'caches',
                            'EmscriptenHostSwift.cmake')

    def _lld_shim_path(self):
        return os.path.join(self.source_dir, 'cmake', 'caches',
                            'EmscriptenHostSwiftLLDShim.cmake')
