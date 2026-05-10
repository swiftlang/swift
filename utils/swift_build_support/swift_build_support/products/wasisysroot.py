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

import os
import sys

from . import cmake_product
from . import llvm
from . import product


class WASISysroot(product.Product):
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
        # WASI sysroot should always be built if standard library is being
        # built for WebAssembly.
        return self.args.build_wasistdlib

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return False

    def build(self, host_target):
        self._build_target(
            host_target,
            enable_wasi_threads=False,
            compiler_rt_os_dir='wasip1',
            target_triple='wasm32-wasip1')
        self._build_target(
            host_target,
            enable_wasi_threads=True,
            compiler_rt_os_dir='wasip1',
            target_triple='wasm32-wasip1-threads')

    def _toolchain_paths(self, host_target):
        if self.args.build_runtime_with_host_compiler:
            cc_path = self.toolchain.cc
            cxx_path = self.toolchain.cxx
            ar_path = self.toolchain.llvm_ar
            nm_path = self.toolchain.llvm_nm
            ranlib_path = self.toolchain.llvm_ranlib

            if not ar_path:
                print(f"error: `llvm-ar` not found for LLVM toolchain at {cc_path}, "
                      "select a toolchain that has `llvm-ar` included", file=sys.stderr)
                sys.exit(1)
        else:
            build_root = os.path.dirname(self.build_dir)
            llvm_build_bin_dir = os.path.join(
                '..', build_root, '%s-%s' % ('llvm', host_target), 'bin')
            native_llvm_tools_path = self.args.native_llvm_tools_path
            native_clang_tools_path = self.args.native_clang_tools_path
            llvm_tools_path = native_llvm_tools_path or llvm_build_bin_dir
            clang_tools_path = native_clang_tools_path or llvm_build_bin_dir
            ar_path = os.path.join(llvm_tools_path, 'llvm-ar')
            nm_path = os.path.join(llvm_tools_path, 'llvm-nm')
            ranlib_path = os.path.join(llvm_tools_path, 'llvm-ranlib')
            cc_path = os.path.join(clang_tools_path, 'clang')
            cxx_path = os.path.join(clang_tools_path, 'clang++')

        return cc_path, cxx_path, ar_path, nm_path, ranlib_path

    def _build_target(self, host_target, enable_wasi_threads, compiler_rt_os_dir, target_triple):
        build_root = os.path.dirname(self.build_dir)
        target_build_dir = os.path.join(self.build_dir, target_triple)
        wasi_libc_build_dir = os.path.join(target_build_dir, 'wasi-libc')
        runtimes_build_dir = os.path.join(target_build_dir, 'runtimes')
        compiler_rt_builtins_build_dir = os.path.join(target_build_dir, 'compiler-rt-builtins')
        compiler_rt_build_dir = os.path.join(target_build_dir, 'compiler-rt')

        cc_path, cxx_path, ar_path, nm_path, ranlib_path = self._toolchain_paths(host_target)

        cmake_has_threads = 'TRUE' if enable_wasi_threads else 'FALSE'
        sysroot_install_path = WASISysroot.sysroot_install_path(build_root, target_triple)

        c_flags = []
        # Explicitly disable exceptions even though it's usually implicitly disabled by
        # LIBCXX_ENABLE_EXCEPTIONS because the CMake feature check fails to detect
        # -fno-exceptions support in clang due to missing compiler-rt while configuring
        # as mono project.
        cxx_flags = ['-fno-exceptions']
        if enable_wasi_threads:
            c_flags.append('-pthread')
            cxx_flags.append('-pthread')
        c_flags_str = ' '.join(c_flags)
        cxx_flags_str = ' '.join(cxx_flags)

        # Build compiler-rt builtins first and pass the built archive to wasi-libc.
        self._build_compiler_rt_builtins(
            compiler_rt_build_dir=compiler_rt_builtins_build_dir,
            target_triple=target_triple,
            compiler_rt_os_dir=compiler_rt_os_dir,
            build_root=build_root,
            sysroot_install_path=sysroot_install_path,
            cc_path=cc_path,
            cxx_path=cxx_path,
            ar_path=ar_path,
            ranlib_path=ranlib_path,
            c_flags=c_flags_str,
            cxx_flags=cxx_flags_str)

        builtins_lib_path = os.path.join(
            compiler_rt_builtins_build_dir, 'lib', compiler_rt_os_dir, 'libclang_rt.builtins-wasm32.a')
        if not os.path.isfile(builtins_lib_path):
            print(
                f"error: `libclang_rt.builtins-wasm32.a` not found at {builtins_lib_path}",
                file=sys.stderr)
            sys.exit(1)

        self._build_wasi_libc(
            wasi_libc_build_dir=wasi_libc_build_dir,
            target_triple=target_triple,
            build_root=build_root,
            cc_path=cc_path,
            ar_path=ar_path,
            nm_path=nm_path,
            ranlib_path=ranlib_path,
            builtins_lib_path=builtins_lib_path)

        self._build_compiler_rt(
            compiler_rt_build_dir=compiler_rt_build_dir,
            target_triple=target_triple,
            compiler_rt_os_dir=compiler_rt_os_dir,
            build_root=build_root,
            sysroot_install_path=sysroot_install_path,
            cc_path=cc_path,
            cxx_path=cxx_path,
            ar_path=ar_path,
            ranlib_path=ranlib_path,
            c_flags=c_flags_str,
            cxx_flags=cxx_flags_str)

        self._build_runtimes(
            runtimes_build_dir=runtimes_build_dir,
            target_triple=target_triple,
            sysroot_install_path=sysroot_install_path,
            build_root=build_root,
            cc_path=cc_path,
            cxx_path=cxx_path,
            ar_path=ar_path,
            ranlib_path=ranlib_path,
            cmake_has_threads=cmake_has_threads,
            c_flags=c_flags_str,
            cxx_flags=cxx_flags_str)

    def _build_wasi_libc(self, wasi_libc_build_dir, target_triple, build_root,
                         cc_path, ar_path, nm_path, ranlib_path, builtins_lib_path):
        cmake = cmake_product.CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=self.source_dir,
            build_dir=wasi_libc_build_dir)

        cmake.cmake_options.define('CMAKE_INSTALL_PREFIX:PATH', '/')
        # Keep sysroot layout under <sysroot>/{include,lib}. GNUInstallDirs
        # defaults to usr/{include,lib} when prefix is '/', which causes
        # duplicated headers/libs and an extra top-level `usr` in WASI.sdk.
        cmake.cmake_options.define('CMAKE_INSTALL_INCLUDEDIR:PATH', '/include')
        cmake.cmake_options.define('CMAKE_INSTALL_LIBDIR:PATH', '/lib')
        cmake.cmake_options.define('CMAKE_BUILD_TYPE:STRING', self.args.build_variant)
        cmake.cmake_options.define('CMAKE_C_COMPILER:FILEPATH', cc_path)
        cmake.cmake_options.define('CMAKE_C_COMPILER_TARGET:STRING', target_triple)
        # wasi-libc IS the C library, so there's nothing to link against during
        # CMake's compiler check. Skip it.
        cmake.cmake_options.define('CMAKE_C_COMPILER_WORKS:BOOL', 'TRUE')
        cmake.cmake_options.define('CMAKE_AR:FILEPATH', ar_path)
        cmake.cmake_options.define('CMAKE_NM:FILEPATH', nm_path)
        cmake.cmake_options.define('CMAKE_RANLIB:FILEPATH', ranlib_path)
        cmake.cmake_options.define('TARGET_TRIPLE:STRING', target_triple)
        cmake.cmake_options.define('BUILTINS_LIB:FILEPATH', builtins_lib_path)

        cmake.build_with_cmake([], cmake.args.build_variant, [],
                               prefer_native_toolchain=not self.args.build_runtime_with_host_compiler,
                               ignore_extra_cmake_options=True)
        sysroot_install_path = WASISysroot.sysroot_install_path(build_root, target_triple)
        cmake.install_with_cmake(['install'], sysroot_install_path)

    @classmethod
    def sysroot_build_path(cls, build_root, host_target, target_triple):
        """
        Returns the path to wasi-libc's build sysroot, which contains only
        wasi-libc artifacts (without LLVM runtimes).
        """
        return os.path.join(build_root,
                            '%s-%s' % (cls.product_name(), host_target),
                            target_triple, 'wasi-libc', 'sysroot')

    @classmethod
    def sysroot_install_path(cls, build_root, target_triple):
        """
        Returns the path to the sysroot install directory, which contains artifacts
        of wasi-libc and LLVM runtimes.
        """
        return os.path.join(build_root, 'wasi-sysroot', target_triple, 'sysroot')

    @classmethod
    def resource_dir_install_path(cls, build_root, target_triple):
        """
        Returns the path to the compiler resource directory install location.
        """
        return os.path.join(build_root, 'wasi-sysroot', target_triple, 'resource-dir')

    def _build_runtimes(self, runtimes_build_dir, target_triple, sysroot_install_path,
                        build_root, cc_path, cxx_path, ar_path, ranlib_path,
                        cmake_has_threads, c_flags, cxx_flags):
        cmake = cmake_product.CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(os.path.dirname(self.source_dir), 'llvm-project', 'runtimes'),
            build_dir=runtimes_build_dir)

        self._apply_wasi_toolchain_options(
            cmake.cmake_options, sysroot_install_path, target_triple,
            cc_path, cxx_path, ar_path, ranlib_path, c_flags, cxx_flags)
        enable_runtimes = ['libcxx', 'libcxxabi']
        cmake.cmake_options.define('LLVM_ENABLE_RUNTIMES:STRING',
                                   ';'.join(enable_runtimes))

        libdir_suffix = '/' + target_triple
        cmake.cmake_options.define('LIBCXX_LIBDIR_SUFFIX:STRING', libdir_suffix)
        cmake.cmake_options.define('LIBCXXABI_LIBDIR_SUFFIX:STRING', libdir_suffix)
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
            ["install"], WASISysroot.sysroot_install_path(build_root, target_triple))

    def _build_compiler_rt(self, compiler_rt_build_dir, target_triple, compiler_rt_os_dir,
                           build_root, sysroot_install_path,
                           cc_path, cxx_path, ar_path, ranlib_path,
                           c_flags, cxx_flags):
        compiler_rt_source_dir = os.path.join(
            os.path.dirname(self.source_dir), 'llvm-project', 'compiler-rt')
        compiler_rt = cmake_product.CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=compiler_rt_source_dir,
            build_dir=compiler_rt_build_dir)

        self._apply_wasi_toolchain_options(
            compiler_rt.cmake_options, sysroot_install_path, target_triple,
            cc_path, cxx_path, ar_path, ranlib_path, c_flags, cxx_flags)

        compiler_rt.cmake_options.define('COMPILER_RT_DEFAULT_TARGET_ARCH:STRING', 'wasm32')
        compiler_rt.cmake_options.define('COMPILER_RT_DEFAULT_TARGET_ONLY:BOOL', 'TRUE')
        compiler_rt.cmake_options.define('COMPILER_RT_BAREMETAL_BUILD:BOOL', 'TRUE')
        compiler_rt.cmake_options.define('COMPILER_RT_BUILD_XRAY:BOOL', 'FALSE')
        compiler_rt.cmake_options.define('COMPILER_RT_BUILD_PROFILE:BOOL', 'TRUE')
        compiler_rt.cmake_options.define('COMPILER_RT_INCLUDE_TESTS:BOOL', 'FALSE')
        compiler_rt.cmake_options.define('COMPILER_RT_HAS_FPIC_FLAG:BOOL', 'FALSE')
        compiler_rt.cmake_options.define('COMPILER_RT_EXCLUDE_ATOMIC_BUILTIN:BOOL', 'FALSE')
        compiler_rt.cmake_options.define('COMPILER_RT_OS_DIR:STRING', compiler_rt_os_dir)

        compiler_rt.build_with_cmake([], compiler_rt.args.build_variant, [],
                                     prefer_native_toolchain=not self.args.build_runtime_with_host_compiler,
                                     ignore_extra_cmake_options=True)
        compiler_rt.install_with_cmake(
            ["install"],
            WASISysroot.resource_dir_install_path(build_root, target_triple))

    def _apply_wasi_toolchain_options(self, cmake_options, sysroot_build_path,
                                      target_triple, cc_path, cxx_path,
                                      ar_path, ranlib_path, c_flags, cxx_flags):
        cmake_options.define('CMAKE_SYSROOT:PATH', sysroot_build_path)
        cmake_options.define('CMAKE_STAGING_PREFIX:PATH', '/')
        cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'WASI')
        cmake_options.define('CMAKE_SYSTEM_PROCESSOR:STRING', 'wasm32')
        cmake_options.define('UNIX:BOOL', 'TRUE')
        cmake_options.define('CMAKE_AR:FILEPATH', ar_path)
        cmake_options.define('CMAKE_RANLIB:FILEPATH', ranlib_path)
        cmake_options.define('CMAKE_C_COMPILER:FILEPATH', cc_path)
        cmake_options.define('CMAKE_CXX_COMPILER:STRING', cxx_path)
        cmake_options.define('CMAKE_C_FLAGS:STRING', c_flags)
        cmake_options.define('CMAKE_CXX_FLAGS:STRING', cxx_flags)
        cmake_options.define('CMAKE_C_COMPILER_TARGET:STRING', target_triple)
        cmake_options.define('CMAKE_CXX_COMPILER_TARGET:STRING', target_triple)
        cmake_options.define('CMAKE_C_COMPILER_WORKS:BOOL', 'TRUE')
        cmake_options.define('CMAKE_CXX_COMPILER_WORKS:BOOL', 'TRUE')

    def _build_compiler_rt_builtins(self, compiler_rt_build_dir, target_triple,
                                    compiler_rt_os_dir, build_root,
                                    sysroot_install_path, cc_path, cxx_path,
                                    ar_path, ranlib_path, c_flags, cxx_flags):
        compiler_rt_source_dir = os.path.join(
            os.path.dirname(self.source_dir), 'llvm-project', 'compiler-rt')
        compiler_rt = cmake_product.CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=compiler_rt_source_dir,
            build_dir=compiler_rt_build_dir)

        self._apply_wasi_toolchain_options(
            compiler_rt.cmake_options, sysroot_install_path, target_triple,
            cc_path, cxx_path, ar_path, ranlib_path, c_flags, cxx_flags)

        compiler_rt.cmake_options.define('COMPILER_RT_DEFAULT_TARGET_ARCH:STRING', 'wasm32')
        compiler_rt.cmake_options.define('COMPILER_RT_DEFAULT_TARGET_ONLY:BOOL', 'TRUE')
        compiler_rt.cmake_options.define('COMPILER_RT_BAREMETAL_BUILD:BOOL', 'TRUE')
        compiler_rt.cmake_options.define('COMPILER_RT_BUILD_BUILTINS:BOOL', 'TRUE')
        compiler_rt.cmake_options.define('COMPILER_RT_BUILD_PROFILE:BOOL', 'FALSE')
        compiler_rt.cmake_options.define('COMPILER_RT_BUILD_CRT:BOOL', 'FALSE')
        compiler_rt.cmake_options.define('COMPILER_RT_BUILD_XRAY:BOOL', 'FALSE')
        compiler_rt.cmake_options.define('COMPILER_RT_INCLUDE_TESTS:BOOL', 'FALSE')
        compiler_rt.cmake_options.define('COMPILER_RT_HAS_FPIC_FLAG:BOOL', 'FALSE')
        compiler_rt.cmake_options.define('COMPILER_RT_EXCLUDE_ATOMIC_BUILTIN:BOOL', 'FALSE')
        compiler_rt.cmake_options.define('COMPILER_RT_OS_DIR:STRING', compiler_rt_os_dir)

        compiler_rt.build_with_cmake([], compiler_rt.args.build_variant, [],
                                     prefer_native_toolchain=not self.args.build_runtime_with_host_compiler,
                                     ignore_extra_cmake_options=True)

    @classmethod
    def get_dependencies(cls):
        return [llvm.LLVM]
