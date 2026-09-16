# swift_build_support/products/wasistdlib.py --------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os

from . import cmake_product
from . import llvm
from . import swift
from . import wasisysroot
from . import wasmkit
from ..helpers import wasmstdlibhelpers


class WASIStdlib(cmake_product.CMakeProduct):
    @classmethod
    def product_source_name(cls):
        return "swift"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return self.args.build_wasistdlib

    def should_test(self, host_target):
        return self.args.test_wasistdlib

    def build(self, host_target):
        target_triple = 'wasm32-wasip1'
        wasmstdlibhelpers.build_stdlib(
            args=self.args, toolchain=self.toolchain,
            source_dir=self.source_dir, build_dir=self.build_dir,
            host_target=host_target, short_triple='wasip1-wasm32',
            append_platform_cmake_options=lambda opts:
                self._append_platform_cmake_options(opts, target_triple))

    def _append_platform_cmake_options(self, cmake_options, target_triple):
        # Teach CMake about the WASI target. (UNIX:BOOL=TRUE, shared by both
        # WASI and Emscripten, is set in wasmstdlibhelpers.build_stdlib.)
        cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'WASI')
        cmake_options.define('SWIFT_WASI_SYSROOT_PATH:STRING',
                             self._wasi_sysroot_path(target_triple))
        cmake_options.define('SWIFT_PRIMARY_VARIANT_SDK:STRING', 'WASI')
        cmake_options.define('SWIFT_SDKS:STRING', 'WASI')
        cmake_options.define(
            'SWIFT_SDK_embedded_ARCH_wasm32_PATH:PATH',
            self._wasi_sysroot_path('wasm32-wasip1'))
        cmake_options.define(
            'SWIFT_SDK_embedded_ARCH_wasm32-unknown-wasip1_PATH:PATH',
            self._wasi_sysroot_path('wasm32-wasip1'))

        lit_test_paths = [
            'IRGen', 'stdlib', 'Concurrency/Runtime', 'embedded', 'AutoDiff', 'DebugInfo',
            'SILOptimizer',
            # TODO(katei): Enable all interpreter tests
            'Interpreter/enum.swift',
        ]
        lit_test_paths = [os.path.join(
            self.build_dir, 'test-wasi-wasm32', path)
            for path in lit_test_paths]
        cmake_options.define('SWIFT_LIT_TEST_PATHS:STRING',
                             ';'.join(lit_test_paths))

        test_driver_options = [
            # compiler-rt is not installed in the final toolchain, so use one
            # in build dir
            '-Xclang-linker',
            '-resource-dir=' + self._wasi_resource_dir_path(target_triple),
        ]
        # Leading space is needed to separate from other options
        cmake_options.define('SWIFT_DRIVER_TEST_OPTIONS:STRING',
                             ' ' + ' '.join(test_driver_options))

        # The threads subclass overrides these below, so its (unserved) stdlib is left unmapped.
        prefix_map = wasisysroot.file_prefix_map()
        if prefix_map:
            cmake_options.define('SWIFT_STDLIB_EXTRA_SWIFT_COMPILE_FLAGS:STRING',
                                 '-file-prefix-map;' + prefix_map)
            cmake_options.define('SWIFT_STDLIB_EXTRA_C_COMPILE_FLAGS:STRING',
                                 '-ffile-prefix-map=' + prefix_map)

        self._append_threading_options(cmake_options)

    def _append_threading_options(self, cmake_options):
        cmake_options.define('SWIFT_THREADING_PACKAGE:STRING', 'none')

    def test(self, host_target):
        self._test(host_target, 'wasm32-wasip1')

    def _test(self, host_target, target_triple):
        build_root = os.path.dirname(self.build_dir)
        bin_paths = [
            os.path.join(self._host_swift_build_dir(host_target), 'bin'),
            os.path.join(self._host_llvm_build_dir(host_target), 'bin'),
            os.environ['PATH']
        ]
        wasmkit_build_path = os.path.join(
            build_root, '%s-%s' % ('wasmkit', host_target))
        wasmkit_bin_path = wasmkit.WasmKit.cli_file_path(wasmkit_build_path)
        if not os.path.exists(wasmkit_bin_path) or not self.should_test_executable():
            test_target = "check-swift-only_non_executable-wasi-wasm32-custom"
        else:
            test_target = "check-swift-wasi-wasm32-custom"
            bin_paths = [os.path.dirname(wasmkit_bin_path)] + bin_paths

        env = {
            'PATH': os.path.pathsep.join(bin_paths),
            # FIXME: WasmKit takes too long to run these exhaustive tests for now
            'LIT_FILTER_OUT':
                '(Concurrency/Runtime/clock.swift|stdlib/StringIndex.swift)',
        }

        # Embedded stdlib is not built for the threads triple, don't include embedded tests for it.
        if target_triple == 'wasm32-wasip1-threads':
            test_targets = [test_target]
        else:
            test_targets = [test_target, 'check-swift-embedded-wasi']

        self.test_with_cmake(None, test_targets, self._build_variant, [], test_env=env)

    def should_test_executable(self) -> bool:
        return True

    @property
    def _build_variant(self):
        return self.args.build_variant

    def _host_llvm_build_dir(self, host_target):
        build_root = os.path.dirname(self.build_dir)
        return os.path.join('..', build_root, '%s-%s' % ('llvm', host_target))

    def _host_swift_build_dir(self, host_target):
        build_root = os.path.dirname(self.build_dir)
        return os.path.join('..', build_root, '%s-%s' % ('swift', host_target))

    def _wasi_sysroot_path(self, target_triple):
        build_root = os.path.dirname(self.build_dir)
        return wasisysroot.WASISysroot.sysroot_install_path(build_root, target_triple)

    def _wasi_resource_dir_path(self, target_triple):
        build_root = os.path.dirname(self.build_dir)
        return wasisysroot.WASISysroot.resource_dir_install_path(build_root, target_triple)

    def should_install(self, host_target):
        return False

    @classmethod
    def get_dependencies(cls):
        return [llvm.LLVM,
                wasisysroot.WASISysroot,
                wasmkit.WasmKit,
                swift.Swift]


class WASIThreadsStdlib(WASIStdlib):
    def build(self, host_target):
        target_triple = 'wasm32-wasip1-threads'
        wasmstdlibhelpers.build_stdlib(
            args=self.args, toolchain=self.toolchain,
            source_dir=self.source_dir, build_dir=self.build_dir,
            host_target=host_target, short_triple='wasip1-threads-wasm32',
            append_platform_cmake_options=lambda opts:
                self._append_platform_cmake_options(opts, target_triple))

    def test(self, host_target):
        self._test(host_target, 'wasm32-wasip1-threads')

    def should_test_executable(self):
        # TODO(katei): Enable tests once WasmKit supports WASI threads
        return False

    def _append_threading_options(self, cmake_options):
        cmake_options.define('SWIFT_THREADING_PACKAGE:STRING', 'pthreads')
        cmake_options.define('SWIFT_STDLIB_EXTRA_C_COMPILE_FLAGS:STRING',
                             '-mthread-model;posix;-pthread;'
                             '-ftls-model=local-exec')
        cmake_options.define('SWIFT_STDLIB_EXTRA_SWIFT_COMPILE_FLAGS:STRING',
                             '-Xcc;-matomics;-Xcc;-mbulk-memory;'
                             '-Xcc;-mthread-model;-Xcc;posix;'
                             '-Xcc;-pthread;-Xcc;-ftls-model=local-exec')
        cmake_options.define('SWIFT_ENABLE_WASI_THREADS:BOOL', 'TRUE')
