# swift_build_support/products/emscriptenstdlib.py ----------------*- python -*-
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
import shutil

from . import cmake_product
from . import emscriptensysroot
from . import llvm
from . import swift
from .. import cmake
from .. import shell


class EmscriptenStdlib(cmake_product.CMakeProduct):
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
        return self.args.build_emscriptenstdlib

    def should_test(self, host_target):
        return self.args.test_emscriptenstdlib

    def build(self, host_target):
        self._build(host_target, 'wasm32-emscripten', 'emscripten-wasm32')

    def _build(self, host_target, target_triple, short_triple):
        llvm_build_dir = self._configure_llvm(host_target, target_triple, short_triple)
        llvm_cmake_dir = os.path.join(llvm_build_dir, 'lib', 'cmake', 'llvm')
        self._build_stdlib(host_target, target_triple, llvm_cmake_dir)

    def _configure_llvm(self, host_target, target_triple, short_triple):
        # Configure LLVM for WebAssembly target independently
        # from the native LLVM build to turn off zlib and libxml2
        build_root = os.path.dirname(self.build_dir)
        build_dir = os.path.join(
            build_root, 'llvm-%s' % short_triple)
        llvm_source_dir = os.path.join(
            os.path.dirname(self.source_dir), 'llvm-project', 'llvm')

        cmake_options, _, relevant_options = self.host_cmake_options(host_target)
        cmake_options.define('CMAKE_BUILD_TYPE:STRING', self._build_variant)
        cmake_options.define('CMAKE_C_COMPILER_WORKS:BOOL', 'TRUE')
        cmake_options.define('CMAKE_CXX_COMPILER_WORKS:BOOL', 'TRUE')
        cmake_options.define('LLVM_COMPILER_CHECKED:BOOL', 'TRUE')
        cmake_options.define('LLVM_ENABLE_ZLIB:BOOL', 'FALSE')
        cmake_options.define('LLVM_ENABLE_LIBXML2:BOOL', 'FALSE')
        cmake_options.define('LLVM_ENABLE_LIBEDIT:BOOL', 'FALSE')
        cmake_options.define('LLVM_ENABLE_TERMINFO:BOOL', 'FALSE')
        if self.args.build_runtime_with_host_compiler:
            cmake_options.define('CMAKE_ASM_COMPILER:PATH', self.toolchain.cc)

        llvm_cmake = cmake.CMake(
            self.args, self.toolchain,
            prefer_native_toolchain=not self.args.build_runtime_with_host_compiler)
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
        # Set up Emscripten as the target platform
        self.cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'Emscripten')
        self.cmake_options.define('CMAKE_SYSTEM_PROCESSOR:STRING', 'wasm32')
        self.cmake_options.define('UNIX:BOOL', 'TRUE')
        self.cmake_options.define(
            'SWIFT_STDLIB_BUILD_TYPE:STRING', self._build_variant)

        if self.args.build_runtime_with_host_compiler:
            self.cmake_options.define(
                'SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER:BOOL', "TRUE")
            self.cmake_options.define(
                'SWIFT_NATIVE_CLANG_TOOLS_PATH:STRING',
                os.path.dirname(self.toolchain.cc))
            self.cmake_options.define(
                'SWIFT_NATIVE_SWIFT_TOOLS_PATH:STRING',
                os.path.dirname(self.toolchain.swiftc))
            self.cmake_options.define(
                'SWIFT_NATIVE_LLVM_TOOLS_PATH:STRING',
                os.path.dirname(self.toolchain.llvm_ar))
        else:
            toolchain_path = self.native_toolchain_path(host_target)
            self.cmake_options.define('CMAKE_AR:STRING', os.path.join(
                toolchain_path, 'bin', 'llvm-ar'))
            self.cmake_options.define('CMAKE_RANLIB:STRING', os.path.join(
                toolchain_path, 'bin', 'llvm-ranlib'))
            self.cmake_options.define(
                'SWIFT_NATIVE_CLANG_TOOLS_PATH:STRING',
                os.path.join(toolchain_path, 'bin'))
            self.cmake_options.define(
                'SWIFT_NATIVE_SWIFT_TOOLS_PATH:STRING',
                os.path.join(toolchain_path, 'bin'))
            self.cmake_options.define(
                'SWIFT_NATIVE_LLVM_TOOLS_PATH:STRING',
                os.path.join(toolchain_path, 'bin'))
            self.cmake_options.define(
                'BOOTSTRAPPING_MODE:STRING', 'CROSSCOMPILE')
            self.cmake_options.define(
                'SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER:BOOL', 'FALSE')

        self.cmake_options.define('SWIFT_EMSCRIPTEN_SYSROOT_PATH:STRING',
                                  self._emscripten_sysroot_path(target_triple))

        # Skip compiler health checks since compiler-rt for Emscripten
        # is not installed in the host toolchain
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
        self.cmake_options.define('SWIFT_PRIMARY_VARIANT_SDK:STRING', 'EMSCRIPTEN')
        self.cmake_options.define('SWIFT_PRIMARY_VARIANT_ARCH:STRING', 'wasm32')
        self.cmake_options.define('SWIFT_SDKS:STRING', 'EMSCRIPTEN')
        self.cmake_options.define('SWIFT_BUILD_STATIC_STDLIB:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_BUILD_DYNAMIC_STDLIB:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_BUILD_STATIC_SDK_OVERLAY:BOOL', 'TRUE')
        self.cmake_options.define(
            'SWIFT_STDLIB_INSTALL_ONLY_CLANG_RESOURCE_HEADERS:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_STDLIB_STABLE_ABI:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_STDLIB_TRACING:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_STDLIB_HAS_ASLR:BOOL', 'FALSE')
        self.cmake_options.define(
            'SWIFT_STDLIB_INSTALL_PARENT_MODULE_FOR_SHIMS:BOOL', 'FALSE')
        self.cmake_options.define(
            'SWIFT_RUNTIME_CRASH_REPORTER_CLIENT:BOOL', 'FALSE')
        self.cmake_options.define(
            'SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_DISPATCH:BOOL', 'FALSE')
        self.cmake_options.define(
            'SWIFT_STDLIB_SUPPORTS_BACKTRACE_REPORTING:BOOL', 'FALSE')
        self.cmake_options.define('SWIFT_STDLIB_HAS_DLADDR:BOOL', 'FALSE')
        self.cmake_options.define(
            'SWIFT_STDLIB_COMPACT_ABSOLUTE_FUNCTION_POINTER:BOOL', 'TRUE')
        self.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY:BOOL', 'TRUE')
        self.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED:BOOL', 'TRUE')
        self.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_PATH_TO_STRING_PROCESSING_SOURCE:PATH',
                                  os.path.join(self.source_dir, '..',
                                               'swift-experimental-string-processing'))
        self.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_CXX_INTEROP:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_SYNCHRONIZATION:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_VOLATILE:BOOL', 'TRUE')
        self.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION:BOOL', 'TRUE')
        self.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING', 'TRUE')

        self.cmake_options.define(
            'SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB:BOOL', 'TRUE')
        self.cmake_options.define(
            'SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_CROSS_COMPILING', 'TRUE')
        self.cmake_options.define(
            'SWIFT_SDK_embedded_ARCH_wasm32_PATH:PATH',
            self._emscripten_sysroot_path("wasm32-emscripten"))
        self.cmake_options.define(
            'SWIFT_SDK_embedded_ARCH_wasm32-unknown-emscripten_PATH:PATH',
            self._emscripten_sysroot_path("wasm32-emscripten"))

        self.add_extra_cmake_options()

        # Test configuration
        self.cmake_options.define('SWIFT_INCLUDE_TESTS:BOOL', 'TRUE')
        self.cmake_options.define('SWIFT_ENABLE_SOURCEKIT_TESTS:BOOL', 'FALSE')
        lit_test_paths = [
            'IRGen', 'stdlib', 'Concurrency/Runtime', 'embedded',
            'AutoDiff', 'DebugInfo',
        ]
        lit_test_paths = [os.path.join(
            self.build_dir, 'test-emscripten-wasm32', path) for path in lit_test_paths]
        self.cmake_options.define('SWIFT_LIT_TEST_PATHS:STRING',
                                  ';'.join(lit_test_paths))
        self.cmake_options.define('LLVM_LIT_ARGS', self.args.lit_args)
        test_driver_options = [
            '-Xlinker-driver',
            '-resource-dir=' + self._emscripten_resource_dir_path(target_triple),
        ]
        self.cmake_options.define('SWIFT_DRIVER_TEST_OPTIONS:STRING',
                                  ' ' + ' '.join(test_driver_options))

        self.build_with_cmake([], self._build_variant, [],
                              prefer_native_toolchain=not self.args.build_runtime_with_host_compiler)

    def add_extra_cmake_options(self):
        self.cmake_options.define('SWIFT_THREADING_PACKAGE:STRING', 'none')
        # Emscripten keeps compatibility headers (e.g. xlocale.h) in a
        # separate include/compat directory that emcc adds automatically.
        # Since we use raw clang, we need to add it ourselves.
        sysroot = self._emscripten_sysroot_path('wasm32-emscripten')
        compat_include = os.path.join(sysroot, 'include', 'compat')
        self.cmake_options.define('SWIFT_STDLIB_EXTRA_C_COMPILE_FLAGS:STRING',
                                  '-isystem;' + compat_include)

    def test(self, host_target):
        self._test(host_target, 'wasm32-emscripten')

    def _test(self, host_target, target_triple):
        build_root = os.path.dirname(self.build_dir)
        bin_paths = [
            os.path.join(self._host_swift_build_dir(host_target), 'bin'),
            os.path.join(self._host_llvm_build_dir(host_target), 'bin'),
            os.environ['PATH']
        ]

        # Emscripten executables are .js files run by Node.js, not WasmKit
        if shutil.which('node') is not None:
            test_target = "check-swift-emscripten-wasm32-custom"
        else:
            test_target = "check-swift-only_non_executable-emscripten-wasm32-custom"

        env = {
            'PATH': os.path.pathsep.join(bin_paths),
        }

        test_targets = [test_target, 'check-swift-embedded-emscripten']

        self.test_with_cmake(None, test_targets, self._build_variant, [],
                             test_env=env)

    @property
    def _build_variant(self):
        return self.args.build_variant

    def _host_llvm_build_dir(self, host_target):
        build_root = os.path.dirname(self.build_dir)
        return os.path.join('..', build_root, '%s-%s' % ('llvm', host_target))

    def _host_swift_build_dir(self, host_target):
        build_root = os.path.dirname(self.build_dir)
        return os.path.join('..', build_root, '%s-%s' % ('swift', host_target))

    def _emscripten_sysroot_path(self, target_triple):
        build_root = os.path.dirname(self.build_dir)
        return emscriptensysroot.EmscriptenSysroot.sysroot_install_path(
            build_root, target_triple)

    def _emscripten_resource_dir_path(self, target_triple):
        build_root = os.path.dirname(self.build_dir)
        return emscriptensysroot.EmscriptenSysroot.resource_dir_install_path(
            build_root, target_triple)

    def should_install(self, host_target):
        return False

    @classmethod
    def get_dependencies(cls):
        return [llvm.LLVM,
                emscriptensysroot.EmscriptenSysroot,
                emscriptensysroot.EmscriptenLLVMRuntimeLibs,
                swift.Swift]
