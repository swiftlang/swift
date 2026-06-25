# swift_build_support/products/emscriptenstdlib.py ----------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023-2026 Apple Inc. and the Swift project authors
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
from ..helpers import wasmstdlibhelpers


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
        target_triple = 'wasm32-emscripten'
        wasmstdlibhelpers.build_stdlib(
            args=self.args, toolchain=self.toolchain,
            source_dir=self.source_dir, build_dir=self.build_dir,
            host_target=host_target, short_triple='emscripten-wasm32',
            append_platform_cmake_options=lambda opts:
                self._append_platform_cmake_options(opts, target_triple))

    def _append_platform_cmake_options(self, cmake_options, target_triple):
        cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'Emscripten')
        cmake_options.define('SWIFT_EMSCRIPTEN_SYSROOT_PATH:STRING',
                             self._emscripten_sysroot_path(target_triple))
        cmake_options.define('SWIFT_PRIMARY_VARIANT_SDK:STRING', 'EMSCRIPTEN')
        cmake_options.define('SWIFT_SDKS:STRING', 'EMSCRIPTEN')
        cmake_options.define(
            'SWIFT_SDK_embedded_ARCH_wasm32_PATH:PATH',
            self._emscripten_sysroot_path('wasm32-emscripten'))
        cmake_options.define(
            'SWIFT_SDK_embedded_ARCH_wasm32-unknown-emscripten_PATH:PATH',
            self._emscripten_sysroot_path('wasm32-emscripten'))

        lit_test_paths = [
            'IRGen', 'stdlib', 'Concurrency/Runtime', 'embedded',
            'AutoDiff', 'DebugInfo',
        ]
        lit_test_paths = [os.path.join(
            self.build_dir, 'test-emscripten-wasm32', path)
            for path in lit_test_paths]
        cmake_options.define('SWIFT_LIT_TEST_PATHS:STRING',
                             ';'.join(lit_test_paths))

        test_driver_options = [
            '-Xlinker-driver',
            '-resource-dir=' +
            self._emscripten_resource_dir_path(target_triple),
        ]
        cmake_options.define('SWIFT_DRIVER_TEST_OPTIONS:STRING',
                             ' ' + ' '.join(test_driver_options))

        cmake_options.define('SWIFT_THREADING_PACKAGE:STRING', 'none')
        # Emscripten keeps compatibility headers (e.g. xlocale.h) in a
        # separate include/compat directory that emcc adds automatically.
        # Since we use raw clang, we need to add it ourselves.
        sysroot = self._emscripten_sysroot_path('wasm32-emscripten')
        compat_include = os.path.join(sysroot, 'include', 'compat')
        cmake_options.define('SWIFT_STDLIB_EXTRA_C_COMPILE_FLAGS:STRING',
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
