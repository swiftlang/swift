# swift_build_support/products/emscriptenhostllvm.py ------------*- python -*-
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

from . import llvm
from . import product
from .. import shell


class EmscriptenHostLLVM(product.Product):
    @classmethod
    def product_source_name(cls):
        return os.path.join("llvm-project", "llvm")

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def get_dependencies(cls):
        return [llvm.LLVM]

    def should_build(self, host_target):
        return self.args.build_emscripten_host_llvm and \
            not self.is_cross_compile_target(host_target)

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return False

    def build(self, host_target):
        emcmake = self._emcmake_path()
        cache_file = self._cache_file_path()
        llvm_tblgen = self._native_llvm_tblgen(host_target)
        clang_tblgen = self._native_clang_tblgen(host_target)

        if not self.args.dry_run:
            if not os.path.isfile(cache_file):
                print('error: EmscriptenHostLLVM.cmake cache not found at %s'
                      % cache_file, file=sys.stderr)
                sys.exit(1)
            if not os.path.isfile(llvm_tblgen):
                print('error: native llvm-tblgen not found at %s; build native '
                      'LLVM (e.g. --llvm-ninja-targets=llvm-tblgen) or pass '
                      '--native-llvm-tools-path' % llvm_tblgen, file=sys.stderr)
                sys.exit(1)
            if not os.path.isfile(clang_tblgen):
                print('error: native clang-tblgen not found at %s; build it (e.g. '
                      '--llvm-ninja-targets clang-tblgen) or pass '
                      '--native-clang-tools-path' % clang_tblgen, file=sys.stderr)
                sys.exit(1)

        configure_cmd = [
            emcmake, self.toolchain.cmake,
            '-G', 'Ninja',
            '-S', self.source_dir,
            '-B', self.build_dir,
            '-C', cache_file,
            '-DLLVM_TABLEGEN=' + llvm_tblgen,
            '-DCLANG_TABLEGEN=' + clang_tblgen,
            '-DCMAKE_BUILD_TYPE=' + self.args.llvm_build_variant,
        ]

        # A half-finished configure leaves CMakeCache.txt without build.ninja,
        # which wedges every later build; reconfigure if either is missing.
        # EmscriptenHostLLVM.cmake is a `-C` initial cache whose
        # `set(... CACHE ...)` is no-FORCE: editing it has no effect on an
        # existing build dir, even with --reconfigure. Remove the dir first.
        cmake_cache = os.path.join(self.build_dir, 'CMakeCache.txt')
        build_ninja = os.path.join(self.build_dir, 'build.ninja')
        if self.args.reconfigure or not os.path.isfile(cmake_cache) \
                or not os.path.isfile(build_ninja):
            shell.makedirs(self.build_dir)
            shell.call(configure_cmd)

        if self.args.skip_build:
            return

        build_args = ['-j', str(self.args.build_jobs)]
        if self.args.verbose_build:
            build_args.append('-v')
        # llvm-driver is a multicall binary (clang + wasm-ld + utils);
        # building it transitively builds the clang/lld/LLVM libs.
        build_targets = ['llvm-driver']
        shell.call([self.toolchain.cmake, '--build', self.build_dir, '--']
                   + build_args + build_targets)

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

    def _cache_file_path(self):
        # source_dir is <root>/llvm-project/llvm; the cache lives in the
        # sibling swift repo.
        source_root = os.path.dirname(os.path.dirname(self.source_dir))
        return os.path.join(source_root, 'swift', 'cmake', 'caches',
                            'EmscriptenHostLLVM.cmake')

    def _native_llvm_tblgen(self, host_target):
        llvm_bin = os.path.join(self._host_llvm_build_dir(host_target), 'bin')
        native = self.args.native_llvm_tools_path or llvm_bin
        return os.path.join(native, 'llvm-tblgen')

    def _native_clang_tblgen(self, host_target):
        llvm_bin = os.path.join(self._host_llvm_build_dir(host_target), 'bin')
        native = self.args.native_clang_tools_path or llvm_bin
        return os.path.join(native, 'clang-tblgen')

    def _host_llvm_build_dir(self, host_target):
        build_root = os.path.dirname(self.build_dir)
        return os.path.join(build_root, '%s-%s' % ('llvm', host_target))
