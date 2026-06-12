# swift_build_support/products/emscriptensysroot.py ---------------*- python -*-
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
import sys

from . import cmake_product
from . import llvm
from . import product
from .. import shell
from ..helpers import wasmsysroothelpers


class EmscriptenSysroot(product.Product):
    @classmethod
    def product_source_name(cls):
        return "emscripten"

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

    def should_install(self, host_target):
        return False

    def build(self, host_target):
        self._build(host_target, target_triple='wasm32-emscripten')

    def _build(self, host_target, target_triple):
        build_root = os.path.dirname(self.build_dir)
        emscripten_path = self.args.emscripten_path or self.source_dir
        embuilder = os.path.join(emscripten_path, 'embuilder.py')

        # Use a build-local cache directory so we don't pollute the
        # user's global Emscripten cache.
        cache_dir = os.path.join(self.build_dir, 'cache')

        env = dict(os.environ)
        env['EM_CACHE'] = cache_dir

        shell.call([
            sys.executable, embuilder, 'build',
            'libc', 'libcompiler_rt', 'libc++', 'libc++abi',
            'crt1', 'libstubs',
            'libdlmalloc',
        ], env=env)

        # Copy the built sysroot to the canonical install path
        sysroot_src = os.path.join(cache_dir, 'sysroot')
        sysroot_dst = EmscriptenSysroot.sysroot_install_path(
            build_root, target_triple)
        if os.path.exists(sysroot_dst):
            shell.rmtree(sysroot_dst)
        shell.copytree(sysroot_src, sysroot_dst)

        # When using a prebuilt Emscripten SDK, EmscriptenLLVMRuntimeLibs is
        # skipped, but lit tests still need libclang_rt.builtins.a in the
        # resource-dir. Create it from embuilder's libcompiler_rt.a.
        resource_dir = EmscriptenSysroot.resource_dir_install_path(
            build_root, target_triple)
        builtins_src = os.path.join(
            sysroot_dst, 'lib', target_triple, 'libcompiler_rt.a')
        builtins_dst_dir = os.path.join(
            resource_dir, 'lib', 'wasm32-unknown-emscripten')
        if os.path.exists(builtins_src):
            os.makedirs(builtins_dst_dir, exist_ok=True)
            shutil.copy2(builtins_src,
                         os.path.join(builtins_dst_dir,
                                      'libclang_rt.builtins.a'))

    @classmethod
    def get_dependencies(cls):
        return [llvm.LLVM]

    @classmethod
    def sysroot_install_path(cls, build_root, target_triple):
        """
        Returns the path to the sysroot install directory, which contains
        the Emscripten system headers and libraries.
        """
        return os.path.join(build_root, 'emscripten-sysroot',
                            target_triple, 'sysroot')

    @classmethod
    def resource_dir_install_path(cls, build_root, target_triple):
        """
        Returns the path to the compiler resource directory install location.
        """
        return os.path.join(build_root, 'emscripten-sysroot',
                            target_triple, 'resource-dir')


class EmscriptenLLVMRuntimeLibs(cmake_product.CMakeProduct):
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
        # When using a prebuilt Emscripten SDK (e.g. from Homebrew), the
        # sysroot already contains libc++, libc++abi, and compiler-rt
        # built by embuilder, so rebuilding from local llvm-project sources
        # is unnecessary and can cause version mismatches.
        return self.args.build_emscriptenstdlib and not self.args.emscripten_path

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return False

    def build(self, host_target):
        self._build(host_target, target_triple='wasm32-emscripten')

    def _build(self, host_target, target_triple):
        target_build_dir = os.path.join(self.build_dir, target_triple)
        runtimes_build_dir = os.path.join(target_build_dir, 'runtimes')
        build_root = os.path.dirname(self.build_dir)

        if self.args.build_runtime_with_host_compiler:
            cc_path = self.toolchain.cc
            cxx_path = self.toolchain.cxx
            ar_path = self.toolchain.llvm_ar
            ranlib_path = self.toolchain.llvm_ranlib

            if not ar_path:
                print(f"error: `llvm-ar` not found for LLVM toolchain at {cc_path}, "
                      "select a toolchain that has `llvm-ar` included",
                      file=sys.stderr)
                sys.exit(1)
        else:
            llvm_build_bin_dir = os.path.join(
                '..', build_root, '%s-%s' % ('llvm', host_target), 'bin')
            native_llvm_tools_path = self.args.native_llvm_tools_path
            native_clang_tools_path = self.args.native_clang_tools_path
            llvm_tools_path = native_llvm_tools_path or llvm_build_bin_dir
            clang_tools_path = native_clang_tools_path or llvm_build_bin_dir
            ar_path = os.path.join(llvm_tools_path, 'llvm-ar')
            ranlib_path = os.path.join(llvm_tools_path, 'llvm-ranlib')
            cc_path = os.path.join(clang_tools_path, 'clang')
            cxx_path = os.path.join(clang_tools_path, 'clang++')

        sysroot_path = EmscriptenSysroot.sysroot_install_path(
            build_root, target_triple)

        c_flags = ''
        cxx_flags = '-fno-exceptions'

        self._build_runtimes(
            runtimes_build_dir=runtimes_build_dir,
            target_triple=target_triple,
            sysroot_path=sysroot_path,
            build_root=build_root,
            cc_path=cc_path,
            cxx_path=cxx_path,
            ar_path=ar_path,
            ranlib_path=ranlib_path,
            c_flags=c_flags,
            cxx_flags=cxx_flags)

        self._build_compiler_rt(
            target_build_dir=target_build_dir,
            target_triple=target_triple,
            build_root=build_root,
            sysroot_path=sysroot_path,
            cc_path=cc_path,
            cxx_path=cxx_path,
            ar_path=ar_path,
            ranlib_path=ranlib_path,
            c_flags=c_flags,
            cxx_flags=cxx_flags)

    def _build_runtimes(self, runtimes_build_dir, target_triple, sysroot_path,
                        build_root, cc_path, cxx_path, ar_path, ranlib_path,
                        c_flags, cxx_flags):
        wasmsysroothelpers.build_runtimes(
            args=self.args,
            toolchain=self.toolchain,
            runtimes_source_dir=self.source_dir,
            runtimes_build_dir=runtimes_build_dir,
            install_path=EmscriptenSysroot.sysroot_install_path(
                build_root, target_triple),
            enable_threads=False,
            libdir_suffix='/' + target_triple,
            append_platform_cmake_options=self._emscripten_cmake_options_configurator(
                sysroot_path, target_triple,
                cc_path, cxx_path, ar_path, ranlib_path, c_flags, cxx_flags))

    def _build_compiler_rt(self, target_build_dir, target_triple,
                           build_root, sysroot_path,
                           cc_path, cxx_path, ar_path, ranlib_path,
                           c_flags, cxx_flags):
        wasmsysroothelpers.build_compiler_rt(
            args=self.args,
            toolchain=self.toolchain,
            compiler_rt_source_dir=os.path.join(
                os.path.dirname(self.source_dir), 'compiler-rt'),
            compiler_rt_build_dir=os.path.join(target_build_dir, 'compiler-rt'),
            install_path=EmscriptenSysroot.resource_dir_install_path(
                build_root, target_triple),
            compiler_rt_os_dir='emscripten',
            append_platform_cmake_options=self._emscripten_cmake_options_configurator(
                sysroot_path, target_triple,
                cc_path, cxx_path, ar_path, ranlib_path, c_flags, cxx_flags))

    def _emscripten_cmake_options_configurator(self, sysroot_path, target_triple,
                                           cc_path, cxx_path, ar_path, ranlib_path,
                                           c_flags, cxx_flags):
        def append(cmake_options):
            cmake_options.define('CMAKE_SYSROOT:PATH', sysroot_path)
            cmake_options.define('CMAKE_STAGING_PREFIX:PATH', '/')
            cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'Emscripten')
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
        return append

    @classmethod
    def get_dependencies(cls):
        return [EmscriptenSysroot, llvm.LLVM]
