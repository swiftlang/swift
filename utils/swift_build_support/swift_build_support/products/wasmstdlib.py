# swift_build_support/products/wasmstdlib.py --------------------*- python -*-
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
from .. import cmake
from .. import shell


class WasmStdlib(cmake_product.CMakeProduct):
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
        return self.args.build_wasmstdlib

    def should_test(self, host_target):
        return self.args.test_wasmstdlib

    def build(self, host_target):
        self._build(host_target, 'wasm32-wasi', 'wasi-wasm32')

    def _build(self, host_target, target_triple, short_triple):
        llvm_build_dir = self._configure_llvm(target_triple, short_triple)
        llvm_cmake_dir = os.path.join(llvm_build_dir, 'lib', 'cmake', 'llvm')
        self._build_stdlib(host_target, target_triple, llvm_cmake_dir)

    def _configure_llvm(self, target_triple, short_triple):
        # Configure LLVM for WebAssembly target independently
        # from the native LLVM build to turn off zlib and libxml2
        build_root = os.path.dirname(self.build_dir)
        build_dir = os.path.join(
            build_root, 'llvm-%s' % short_triple)
        llvm_source_dir = os.path.join(
            os.path.dirname(self.source_dir), 'llvm-project', 'llvm')
        cmake_options = cmake.CMakeOptions()
        cmake_options.define('CMAKE_BUILD_TYPE:STRING', self._build_variant)
        # compiler-rt for WebAssembly target is not installed in the host toolchain
        # so skip compiler health checks here.
        cmake_options.define('CMAKE_C_COMPILER_WORKS:BOOL', 'TRUE')
        cmake_options.define('CMAKE_CXX_COMPILER_WORKS:BOOL', 'TRUE')
        cmake_options.define('LLVM_COMPILER_CHECKED:BOOL', 'TRUE')
        cmake_options.define('LLVM_ENABLE_ZLIB:BOOL', 'FALSE')
        cmake_options.define('LLVM_ENABLE_LIBXML2:BOOL', 'FALSE')
        cmake_options.define('LLVM_ENABLE_LIBEDIT:BOOL', 'FALSE')
        cmake_options.define('LLVM_ENABLE_TERMINFO:BOOL', 'FALSE')

        llvm_cmake = cmake.CMake(
            self.args, self.toolchain, prefer_native_toolchain=True)
        # Only configure LLVM, not build it because we just need
        # LLVM CMake functionalities
        shell.call(["env", self.toolchain.cmake, "-B", build_dir]
                   + list(llvm_cmake.common_options(self))
                   + list(cmake_options)
                   + [llvm_source_dir])
        return build_dir

    def _build_stdlib(self, host_target, target_triple, llvm_cmake_dir):
        self.cmake_options.define('CMAKE_INSTALL_PREFIX:PATH', '/usr')
        self.cmake_options.define('CMAKE_BUILD_TYPE:STRING', self._build_variant)
        # Teach about the WebAssembly target. UNIX is explicitly set to TRUE
        # as CMake still doesn't recognize WASI as a UNIX platform and the
        # variable is used in LLVM CMake configuration.
        self.cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'WASI')
        self.cmake_options.define('CMAKE_SYSTEM_PROCESSOR:STRING', 'wasm32')
        self.cmake_options.define('UNIX:BOOL', 'TRUE')
        self.cmake_options.define(
            'SWIFT_STDLIB_BUILD_TYPE:STRING', self._build_variant)

        # Toolchain configuration
        toolchain_path = self.native_toolchain_path(host_target)
        # Explicitly set the CMake AR and RANLIB to force it to use llvm-ar/llvm-ranlib
        # instead of the system ar/ranlib, which usually don't support WebAssembly
        # object files.
        self.cmake_options.define('CMAKE_AR:STRING', os.path.join(
            toolchain_path, 'bin', 'llvm-ar'))
        self.cmake_options.define('CMAKE_RANLIB:STRING', os.path.join(
            toolchain_path, 'bin', 'llvm-ranlib'))
        self.cmake_options.define(
            'SWIFT_NATIVE_CLANG_TOOLS_PATH:STRING', os.path.join(toolchain_path, 'bin'))
        self.cmake_options.define(
            'SWIFT_NATIVE_SWIFT_TOOLS_PATH:STRING', os.path.join(toolchain_path, 'bin'))
        self.cmake_options.define(
            'SWIFT_NATIVE_LLVM_TOOLS_PATH:STRING', os.path.join(toolchain_path, 'bin'))
        self.cmake_options.define(
            'BOOTSTRAPPING_MODE:STRING', 'CROSSCOMPILE')
        self.cmake_options.define(
            'SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_WASI_SYSROOT_PATH:STRING',
                                  self._wasi_sysroot_path(target_triple))

        # compiler-rt for WebAssembly target is not installed in the host toolchain
        # so skip compiler health checks here.
        self.cmake_options.define('CMAKE_C_COMPILER_WORKS:BOOL', 'TRUE')
        self.cmake_options.define('CMAKE_CXX_COMPILER_WORKS:BOOL', 'TRUE')
        self.cmake_options.define('CMAKE_Swift_COMPILER_WORKS:BOOL', 'TRUE')
        self.cmake_options.define('LLVM_COMPILER_CHECKED:BOOL', 'TRUE')

        self.cmake_options.define('LLVM_DIR:PATH', llvm_cmake_dir)

        # Standalone stdlib configuration
        self.cmake_options.define('SWIFT_INCLUDE_TOOLS:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_INCLUDE_DOCS:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_BUILD_REMOTE_MIRROR:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_BUILD_SOURCEKIT:BOOL', 'FALSE')

        # Stdlib configuration
        self.cmake_options.define('SWIFT_PRIMARY_VARIANT_SDK:STRING', 'WASI')
        self.cmake_options.define('SWIFT_PRIMARY_VARIANT_ARCH:STRING', 'wasm32')
        self.cmake_options.define('SWIFT_SDKS:STRING', 'WASI')
        # Build only static stdlib
        self.cmake_options.define('SWIFT_BUILD_STATIC_STDLIB:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_BUILD_DYNAMIC_STDLIB:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_BUILD_STATIC_SDK_OVERLAY:BOOL', 'TRUE')
        # TODO: Turn off library evolution once we establish a good way to teach
        # libraries including swift-testing whether to use the stable ABI.
        self.cmake_options.define('SWIFT_STDLIB_STABLE_ABI:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_STDLIB_TRACING:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_STDLIB_HAS_ASLR:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_STDLIB_CONCURRENCY_TRACING:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB:BOOL', 'FALSE')
        self.cmake_options.define(
            'SWIFT_STDLIB_INSTALL_PARENT_MODULE_FOR_SHIMS:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_RUNTIME_CRASH_REPORTER_CLIENT:BOOL', 'FALSE')
        self.cmake_options.define(
            'SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_DISPATCH:BOOL', 'FALSE')
        self.cmake_options.define(
            'SWIFT_STDLIB_SUPPORTS_BACKTRACE_REPORTING:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_STDLIB_HAS_DLADDR:BOOL', 'FALSE')
        self.cmake_options.define(
            'SWIFT_STDLIB_COMPACT_ABSOLUTE_FUNCTION_POINTER:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED:BOOL', 'TRUE')
        self.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_PATH_TO_STRING_PROCESSING_SOURCE:PATH',
                                  os.path.join(self.source_dir, '..',
                                               'swift-experimental-string-processing'))
        self.cmake_options.define('SWIFT_ENABLE_EXPERIMENTAL_CXX_INTEROP:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_SYNCHRONIZATION:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_VOLATILE:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_EXPERIMENTAL_POINTER_BOUNDS:BOOL',
                                  'TRUE')

        self.add_extra_cmake_options()

        # Test configuration
        self.cmake_options.define('SWIFT_INCLUDE_TESTS:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_SOURCEKIT_TESTS:BOOL', 'FALSE')
        lit_test_paths = ['IRGen', 'stdlib', 'Concurrency/Runtime', 'embedded/wasm']
        lit_test_paths = [os.path.join(
            self.build_dir, 'test-wasi-wasm32', path) for path in lit_test_paths]
        self.cmake_options.define('SWIFT_LIT_TEST_PATHS:STRING',
                                  ';'.join(lit_test_paths))
        test_driver_options = [
            # compiler-rt is not installed in the final toolchain, so use one
            # in build dir
            '-Xclang-linker', '-resource-dir=' + self._wasi_sysroot_path(target_triple),
        ]
        # Leading space is needed to separate from other options
        self.cmake_options.define('SWIFT_DRIVER_TEST_OPTIONS:STRING',
                                  ' ' + ' '.join(test_driver_options))

        # Configure with WebAssembly target variant, and build with just-built toolchain
        self.build_with_cmake([], self._build_variant, [],
                              prefer_native_toolchain=True)

    def add_extra_cmake_options(self):
        self.cmake_options.define('SWIFT_THREADING_PACKAGE:STRING', 'none')

    def test(self, host_target):
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
        self.test_with_cmake(None, [test_target], self._build_variant, [], test_env=env)

    def should_test_executable(self):
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
        return wasisysroot.WASILibc.sysroot_install_path(build_root, target_triple)

    def should_install(self, host_target):
        return False

    @classmethod
    def get_dependencies(cls):
        return [llvm.LLVM,
                wasisysroot.WASILibc,
                wasisysroot.WasmLLVMRuntimeLibs,
                wasmkit.WasmKit,
                swift.Swift]


class WasmThreadsStdlib(WasmStdlib):
    def build(self, host_target):
        self._build(host_target, 'wasm32-wasip1-threads', 'wasip1-threads-wasm32')

    def should_test_executable(self):
        # TODO(katei): Enable tests once WasmKit supports WASI threads
        return False

    def add_extra_cmake_options(self):
        self.cmake_options.define('SWIFT_THREADING_PACKAGE:STRING', 'pthreads')
        self.cmake_options.define('SWIFT_STDLIB_EXTRA_C_COMPILE_FLAGS:STRING',
                                  '-mthread-model;posix;-pthread;'
                                  '-ftls-model=local-exec')
        self.cmake_options.define('SWIFT_STDLIB_EXTRA_SWIFT_COMPILE_FLAGS:STRING',
                                  '-Xcc;-matomics;-Xcc;-mbulk-memory;'
                                  '-Xcc;-mthread-model;-Xcc;posix;'
                                  '-Xcc;-pthread;-Xcc;-ftls-model=local-exec')
        self.cmake_options.define('SWIFT_ENABLE_WASI_THREADS:BOOL', 'TRUE')
