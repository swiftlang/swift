# swift_build_support/products/wasisysroot.py -------------------*- python -*-
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

import multiprocessing
import os

from . import cmake_product
from . import llvm
from . import product
from .. import shell


class WASILibc(product.Product):
    @classmethod
    def product_source_name(cls):
        return "wasi-libc"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        # wasi-libc should always be built if standard library is being built for
        # WebAssembly.
        return self.args.build_wasmstdlib

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return False

    def build(self, host_target):
        self._build(host_target)
        self._build(host_target, thread_model='posix',
                    target_triple='wasm32-wasip1-threads')

    def _build(self, host_target, thread_model='single', target_triple='wasm32-wasi'):
        build_root = os.path.dirname(self.build_dir)

        if self.args.build_runtime_with_host_compiler:
            clang_path = self.toolchain.cc
            ar_path = self.toolchain.llvm_ar
            nm_path = self.toolchain.llvm_nm
        else:
            llvm_build_bin_dir = os.path.join(
                '..', build_root, '%s-%s' % ('llvm', host_target), 'bin')
            llvm_tools_path = self.args.native_llvm_tools_path or llvm_build_bin_dir
            clang_tools_path = self.args.native_clang_tools_path or llvm_build_bin_dir
            clang_path = os.path.join(clang_tools_path, 'clang')
            ar_path = os.path.join(llvm_tools_path, 'llvm-ar')
            nm_path = os.path.join(llvm_tools_path, 'llvm-nm')

        build_jobs = self.args.build_jobs or multiprocessing.cpu_count()

        sysroot_build_dir = WASILibc.sysroot_build_path(
            build_root, host_target, target_triple)

        sysroot_install_path = WASILibc.sysroot_install_path(build_root, target_triple)
        shell.call([
            'make', 'install',
            '-j', str(build_jobs),
            '-C', self.source_dir,
            'OBJDIR=' + os.path.join(self.build_dir, 'obj-' + thread_model),
            'SYSROOT=' + sysroot_build_dir,
            'INSTALL_DIR=' + sysroot_install_path,
            'CC=' + clang_path,
            'AR=' + ar_path,
            'NM=' + nm_path,
            'THREAD_MODEL=' + thread_model,
            'TARGET_TRIPLE=' + target_triple,
        ])

        if target_triple == "wasm32-wasi":
            # Alias wasm32-wasip1 to wasm32-wasi as Embedded modules use
            # wasm32-unknown-wasip1 as the target triple.
            for subpath in ["lib", "include"]:
                dest_path = os.path.join(sysroot_install_path, subpath, "wasm32-wasip1")
                if not os.path.exists(dest_path):
                    shell.symlink("wasm32-wasi", dest_path)

            dest_path = os.path.join(sysroot_install_path, "lib", "wasip1")
            if not os.path.exists(dest_path):
                shell.symlink("wasi", dest_path)

    @classmethod
    def get_dependencies(cls):
        return [llvm.LLVM]

    @classmethod
    def sysroot_build_path(cls, build_root, host_target, target_triple):
        """
        Returns the path to the sysroot build directory, which contains only the
        artifacts of wasi-libc (Not including the artifacts of LLVM runtimes).
        """
        return os.path.join(build_root,
                            '%s-%s' % (cls.product_name(), host_target),
                            'sysroot', target_triple)

    @classmethod
    def sysroot_install_path(cls, build_root, target_triple):
        """
        Returns the path to the sysroot install directory, which contains artifacts
        of wasi-libc and LLVM runtimes.
        """
        return os.path.join(build_root, 'wasi-sysroot', target_triple)


class WasmLLVMRuntimeLibs(cmake_product.CMakeProduct):
    @classmethod
    def product_source_name(cls):
        return os.path.join("llvm-project", "runtimes")

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        # LLVM runtime libraries should always be built for WebAssembly
        # if standard library is being built for WebAssembly.
        return self.args.build_wasmstdlib

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return False

    def build(self, host_target):
        self._build(host_target, enable_wasi_threads=False,
                    compiler_rt_os_dir='wasi', target_triple='wasm32-wasi')
        self._build(host_target, enable_wasi_threads=True,
                    compiler_rt_os_dir='wasip1', target_triple='wasm32-wasip1-threads')

    def _build(self, host_target, enable_wasi_threads, compiler_rt_os_dir, target_triple):
        cmake = cmake_product.CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=self.source_dir,
            build_dir=os.path.join(self.build_dir, target_triple))

        build_root = os.path.dirname(self.build_dir)

        if self.args.build_runtime_with_host_compiler:
            cc_path = self.toolchain.cc
            cxx_path = self.toolchain.cxx
            ar_path = self.toolchain.llvm_ar
            ranlib_path = self.toolchain.llvm_ranlib
        else:
            llvm_build_bin_dir = os.path.join(
                '..', build_root, '%s-%s' % ('llvm', host_target), 'bin')
            llvm_tools_path = cmake.args.native_llvm_tools_path or llvm_build_bin_dir
            clang_tools_path = cmake.args.native_clang_tools_path or llvm_build_bin_dir
            ar_path = os.path.join(llvm_tools_path, 'llvm-ar')
            ranlib_path = os.path.join(llvm_tools_path, 'llvm-ranlib')
            cc_path = os.path.join(clang_tools_path, 'clang')
            cxx_path = os.path.join(clang_tools_path, 'clang++')

        cmake_has_threads = 'TRUE' if enable_wasi_threads else 'FALSE'

        cmake.cmake_options.define(
            'CMAKE_SYSROOT:PATH',
            WASILibc.sysroot_build_path(build_root, host_target, target_triple))
        enable_runtimes = ['libcxx', 'libcxxabi', 'compiler-rt']
        cmake.cmake_options.define('LLVM_ENABLE_RUNTIMES:STRING',
                                   ';'.join(enable_runtimes))

        libdir_suffix = '/' + target_triple
        cmake.cmake_options.define('LIBCXX_LIBDIR_SUFFIX:STRING', libdir_suffix)
        cmake.cmake_options.define('LIBCXXABI_LIBDIR_SUFFIX:STRING', libdir_suffix)
        cmake.cmake_options.define('CMAKE_STAGING_PREFIX:PATH', '/')

        cmake.cmake_options.define('COMPILER_RT_DEFAULT_TARGET_ARCH:STRING', 'wasm32')
        cmake.cmake_options.define('COMPILER_RT_DEFAULT_TARGET_ONLY:BOOL', 'TRUE')
        cmake.cmake_options.define('COMPILER_RT_BAREMETAL_BUILD:BOOL', 'TRUE')
        cmake.cmake_options.define('COMPILER_RT_BUILD_XRAY:BOOL', 'FALSE')
        cmake.cmake_options.define('COMPILER_RT_BUILD_PROFILE:BOOL', 'TRUE')
        cmake.cmake_options.define('COMPILER_RT_INCLUDE_TESTS:BOOL', 'FALSE')
        cmake.cmake_options.define('COMPILER_RT_HAS_FPIC_FLAG:BOOL', 'FALSE')
        cmake.cmake_options.define('COMPILER_RT_EXCLUDE_ATOMIC_BUILTIN:BOOL', 'FALSE')
        cmake.cmake_options.define('COMPILER_RT_OS_DIR:STRING', compiler_rt_os_dir)

        cmake.cmake_options.define('CMAKE_C_COMPILER_WORKS:BOOL', 'TRUE')
        cmake.cmake_options.define('CMAKE_CXX_COMPILER_WORKS:BOOL', 'TRUE')

        cmake.cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'WASI')
        cmake.cmake_options.define('CMAKE_SYSTEM_PROCESSOR:STRING', 'wasm32')
        cmake.cmake_options.define('CMAKE_AR:FILEPATH', ar_path)
        cmake.cmake_options.define('CMAKE_RANLIB:FILEPATH', ranlib_path)
        cmake.cmake_options.define('CMAKE_C_COMPILER:FILEPATH', cc_path)
        cmake.cmake_options.define('CMAKE_CXX_COMPILER:STRING', cxx_path)

        c_flags = []
        # Explicitly disable exceptions even though it's usually implicitly disabled by
        # LIBCXX_ENABLE_EXCEPTIONS because the CMake feature check fails to detect
        # -fno-exceptions support in clang due to missing compiler-rt while configuring
        # as mono project.
        cxx_flags = ['-fno-exceptions']
        if enable_wasi_threads:
            c_flags.append('-pthread')
            cxx_flags.append('-pthread')
        cmake.cmake_options.define('CMAKE_C_FLAGS:STRING', ' '.join(c_flags))
        cmake.cmake_options.define('CMAKE_CXX_FLAGS:STRING', ' '.join(cxx_flags))

        cmake.cmake_options.define('CMAKE_C_COMPILER_TARGET:STRING', target_triple)
        cmake.cmake_options.define('CMAKE_CXX_COMPILER_TARGET:STRING', target_triple)

        cmake.cmake_options.define('CXX_SUPPORTS_CXX11:BOOL', 'TRUE')

        cmake.cmake_options.define('LIBCXX_ENABLE_THREADS:BOOL', cmake_has_threads)
        cmake.cmake_options.define('LIBCXX_HAS_PTHREAD_API:BOOL', cmake_has_threads)
        cmake.cmake_options.define('LIBCXX_HAS_EXTERNAL_THREAD_API:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXX_BUILD_EXTERNAL_THREAD_LIBRARY:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXX_HAS_WIN32_THREAD_API:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXX_ENABLE_SHARED:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXX_ENABLE_EXPERIMENTAL_LIBRARY:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXX_ENABLE_EXCEPTIONS:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXX_ENABLE_FILESYSTEM:BOOL', 'TRUE')
        cmake.cmake_options.define('LIBCXX_CXX_ABI', 'libcxxabi')
        cmake.cmake_options.define('LIBCXX_HAS_MUSL_LIBC:BOOL', 'TRUE')

        cmake.cmake_options.define('LIBCXX_ABI_VERSION', '2')
        cmake.cmake_options.define('LIBCXXABI_ENABLE_EXCEPTIONS:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXXABI_ENABLE_SHARED:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXXABI_USE_LLVM_UNWINDER:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXXABI_SILENT_TERMINATE:BOOL', 'TRUE')
        cmake.cmake_options.define('LIBCXXABI_ENABLE_THREADS:BOOL', cmake_has_threads)
        cmake.cmake_options.define('LIBCXXABI_HAS_PTHREAD_API:BOOL', cmake_has_threads)
        cmake.cmake_options.define('LIBCXXABI_HAS_EXTERNAL_THREAD_API:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXXABI_BUILD_EXTERNAL_THREAD_LIBRARY:BOOL',
                                   'FALSE')
        cmake.cmake_options.define('LIBCXXABI_HAS_WIN32_THREAD_API:BOOL', 'FALSE')
        cmake.cmake_options.define('LIBCXXABI_ENABLE_PIC:BOOL', 'FALSE')
        cmake.cmake_options.define('UNIX:BOOL', 'TRUE')

        cmake.build_with_cmake([], cmake.args.build_variant, [],
                               prefer_native_toolchain=not self.args.build_runtime_with_host_compiler,
                               ignore_extra_cmake_options=True)
        cmake.install_with_cmake(
            ["install"], WASILibc.sysroot_install_path(build_root, target_triple))

    @classmethod
    def get_dependencies(cls):
        return [WASILibc, llvm.LLVM]
