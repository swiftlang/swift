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
    _os_environment = 'wasip1'
    _max_memory_bytes = None

    # Clang's multiarch triple names the sysroot and resource-dir layout,
    # Swift's `-target` triple names module directories, and the short triple
    # names the target-only LLVM build directory.
    @classmethod
    def multiarch_triple(cls):
        return 'wasm32-%s' % cls._os_environment

    @classmethod
    def swift_triple(cls):
        return 'wasm32-unknown-%s' % cls._os_environment

    @classmethod
    def short_triple(cls):
        return '%s-wasm32' % cls._os_environment

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
        wasmstdlibhelpers.build_stdlib(
            args=self.args, toolchain=self.toolchain,
            source_dir=self.source_dir, build_dir=self.build_dir,
            host_target=host_target, short_triple=self.short_triple(),
            append_platform_cmake_options=self._append_platform_cmake_options)

    def _append_platform_cmake_options(self, cmake_options):
        sysroot = self._wasi_sysroot_path(self.multiarch_triple())
        # Teach CMake about the WASI target. (UNIX:BOOL=TRUE, shared by both
        # WASI and Emscripten, is set in wasmstdlibhelpers.build_stdlib.)
        cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'WASI')
        cmake_options.define('SWIFT_WASI_SYSROOT_PATH:STRING', sysroot)
        cmake_options.define('SWIFT_PRIMARY_VARIANT_SDK:STRING', 'WASI')
        cmake_options.define('SWIFT_SDKS:STRING', 'WASI')
        cmake_options.define(
            'SWIFT_SDK_embedded_ARCH_wasm32_PATH:PATH', sysroot)
        cmake_options.define(
            'SWIFT_SDK_embedded_ARCH_%s_PATH:PATH' % self.swift_triple(),
            sysroot)

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
            '-resource-dir=' + self._wasi_resource_dir_path(
                self.multiarch_triple()),
        ]
        if self._max_memory_bytes is not None:
            test_driver_options += [
                '-Xclang-linker',
                '-Wl,--max-memory=%d' % self._max_memory_bytes]
        # Read by test/embedded/lit.local.cfg for the embedded suite's link line.
        cmake_options.define(
            'SWIFT_WASI_MAX_MEMORY:STRING',
            '' if self._max_memory_bytes is None
            else str(self._max_memory_bytes))
        # Leading space is needed to separate from other options
        cmake_options.define('SWIFT_DRIVER_TEST_OPTIONS:STRING',
                             ' ' + ' '.join(test_driver_options))

        extra_swift_flags, extra_c_flags = self.threading_compile_flags()
        prefix_map = wasisysroot.file_prefix_map()
        if prefix_map:
            extra_swift_flags = ['-file-prefix-map',
                                 prefix_map] + extra_swift_flags
            extra_c_flags = ['-ffile-prefix-map=' + prefix_map] + extra_c_flags
        # Each of these keys is defined once: a later -D wins, so defining one
        # again in a subclass would drop everything the base put there.
        if extra_swift_flags:
            cmake_options.define(
                'SWIFT_STDLIB_EXTRA_SWIFT_COMPILE_FLAGS:STRING',
                ';'.join(extra_swift_flags))
        if extra_c_flags:
            cmake_options.define('SWIFT_STDLIB_EXTRA_C_COMPILE_FLAGS:STRING',
                                 ';'.join(extra_c_flags))

        self._append_threading_options(cmake_options)

    @classmethod
    def threading_compile_flags(cls):
        """Return `(swift_flags, clang_flags)` making this product's target
        thread-capable. Read by wasiswiftsdk.py for the Swift SDK's own
        libraries, which are built outside the stdlib's CMake."""
        return ([], [])

    def _append_threading_options(self, cmake_options):
        cmake_options.define('SWIFT_THREADING_PACKAGE:STRING', 'none')

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
        can_run_wasm = (os.path.exists(wasmkit_bin_path)
                        and self.should_test_executable())
        if can_run_wasm:
            test_target = "check-swift-wasi-wasm32-custom"
            bin_paths = [os.path.dirname(wasmkit_bin_path)] + bin_paths
        else:
            test_target = "check-swift-only_non_executable-wasi-wasm32-custom"

        env = {
            'PATH': os.path.pathsep.join(bin_paths),
            # FIXME: WasmKit takes too long to run these exhaustive tests for now
            'LIT_FILTER_OUT':
                '(Concurrency/Runtime/clock.swift|stdlib/StringIndex.swift)',
        }

        test_targets = [test_target]
        # No non-executable variant of this target exists to fall back to:
        # test/CMakeLists.txt builds it with one hardcoded test mode.
        if can_run_wasm:
            test_targets.append('check-swift-embedded-wasi')

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
    _os_environment = 'wasip1-threads'
    # wasm-ld pins a shared memory's maximum to its initial size unless a
    # maximum is given, which leaves the guest heap unable to grow. Matches the
    # maximum the shipped Swift SDK's threads toolset sets.
    _max_memory_bytes = 1073741824

    @classmethod
    def threading_compile_flags(cls):
        return (['-Xcc', '-matomics', '-Xcc', '-mbulk-memory',
                 '-Xcc', '-mthread-model', '-Xcc', 'posix',
                 '-Xcc', '-pthread', '-Xcc', '-ftls-model=local-exec'],
                ['-mthread-model', 'posix', '-pthread',
                 '-ftls-model=local-exec'])

    def _append_threading_options(self, cmake_options):
        cmake_options.define('SWIFT_THREADING_PACKAGE:STRING', 'pthreads')
        cmake_options.define('SWIFT_ENABLE_WASI_THREADS:BOOL', 'TRUE')
