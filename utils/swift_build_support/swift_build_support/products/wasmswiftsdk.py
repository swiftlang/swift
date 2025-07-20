# swift_build_support/products/wasmswiftsdk.py ------------------*- python -*-
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

from . import product
from . import wasisysroot
from .wasmstdlib import WasmStdlib, WasmThreadsStdlib
from .cmake_product import CMakeProduct
from .. import shell


class WasmSwiftSDK(product.Product):
    @classmethod
    def product_source_name(cls):
        return "swift-sdk-generator"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return self.args.build_wasmstdlib

    def should_test(self, host_target):
        return False

    def _target_package_path(self, swift_host_triple):
        return os.path.join(self.build_dir, 'Toolchains', swift_host_triple)

    def _append_platform_cmake_options(self, cmake_options, swift_host_triple, has_pthread, wasi_sysroot, extra_swift_flags):
        cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'WASI')
        cmake_options.define('CMAKE_SYSTEM_PROCESSOR:STRING', 'wasm32')
        cmake_options.define('CMAKE_C_COMPILER_TARGET', swift_host_triple)
        cmake_options.define('CMAKE_CXX_COMPILER_TARGET', swift_host_triple)
        cmake_options.define(
            'CMAKE_Swift_COMPILER_TARGET', swift_host_triple)
        cmake_options.define('CMAKE_SYSROOT', wasi_sysroot)

        dest_dir = self._target_package_path(swift_host_triple)
        swift_resource_dir = os.path.join(dest_dir, 'usr', 'lib', 'swift_static')
        clang_resource_dir = os.path.join(swift_resource_dir, 'clang')

        swift_flags = ['-sdk', wasi_sysroot, '-resource-dir',
                       swift_resource_dir] + extra_swift_flags
        c_flags = ['-resource-dir', clang_resource_dir]
        cxx_flags = c_flags + ['-fno-exceptions']
        if has_pthread:
            clang_flags = ['-mthread-model', 'posix', '-pthread']
            c_flags.extend(clang_flags)
            cxx_flags.extend(clang_flags)
            swift_flags.extend(['-Xcc', '-matomics', '-Xcc', '-mbulk-memory',
                               '-Xcc', '-mthread-model', '-Xcc', 'posix', '-Xcc', '-pthread'])

        cmake_options.define('CMAKE_Swift_FLAGS', ' '.join(swift_flags))
        cmake_options.define('CMAKE_C_FLAGS', ' '.join(c_flags))
        cmake_options.define('CMAKE_CXX_FLAGS', ' '.join(cxx_flags))
        cmake_options.define('CMAKE_Swift_COMPILER_FORCED', 'TRUE')
        cmake_options.define('CMAKE_CXX_COMPILER_FORCED', 'TRUE')
        cmake_options.define('CMAKE_BUILD_TYPE', self.args.build_variant)

        # Explicitly choose ar and ranlib from just-built LLVM tools since tools in the host system
        # unlikely support Wasm object format.
        native_toolchain_path = self.native_toolchain_path(self.args.host_target)
        cmake_options.define('CMAKE_AR', os.path.join(
            native_toolchain_path, 'bin', 'llvm-ar'))
        cmake_options.define('CMAKE_RANLIB', os.path.join(
            native_toolchain_path, 'bin', 'llvm-ranlib'))

    def _build_libxml2(self, swift_host_triple, has_pthread, wasi_sysroot):
        libxml2 = CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(
                os.path.dirname(self.source_dir), 'libxml2'),
            build_dir=os.path.join(self.build_dir, 'libxml2', swift_host_triple))
        self._append_platform_cmake_options(
            libxml2.cmake_options, swift_host_triple, has_pthread, wasi_sysroot, [])
        libxml2.cmake_options.define('LIBXML2_WITH_C14N', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_CATALOG', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_DEBUG', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_DOCB', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_FTP', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_HTML', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_HTTP', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_ICONV', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_ICU', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_ISO8859X', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_LEGACY', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_LZMA', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_MEM_DEBUG', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_MODULES', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_OUTPUT', 'TRUE')
        libxml2.cmake_options.define('LIBXML2_WITH_PATTERN', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_PROGRAMS', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_PUSH', 'TRUE')
        libxml2.cmake_options.define('LIBXML2_WITH_PYTHON', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_READER', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_REGEXPS', 'TRUE')
        libxml2.cmake_options.define('LIBXML2_WITH_RUN_DEBUG', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_SAX1', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_SCHEMAS', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_SCHEMATRON', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_TESTS', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_TREE', 'TRUE')
        libxml2.cmake_options.define('LIBXML2_WITH_VALID', 'TRUE')
        libxml2.cmake_options.define('LIBXML2_WITH_WRITER', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_XINCLUDE', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_XPATH', 'TRUE')
        libxml2.cmake_options.define('LIBXML2_WITH_XPTR', 'FALSE')
        libxml2.cmake_options.define('LIBXML2_WITH_ZLIB', 'FALSE')
        libxml2.cmake_options.define('BUILD_SHARED_LIBS', 'FALSE')

        cmake_thread_enabled = 'TRUE' if has_pthread else 'FALSE'
        libxml2.cmake_options.define('LIBXML2_WITH_THREAD_ALLOC', cmake_thread_enabled)
        libxml2.cmake_options.define('LIBXML2_WITH_THREADS', cmake_thread_enabled)
        libxml2.cmake_options.define('HAVE_PTHREAD_H', cmake_thread_enabled)

        libxml2.build_with_cmake([], self.args.build_variant, [],
                                 prefer_native_toolchain=True,
                                 ignore_extra_cmake_options=True)
        with shell.pushd(libxml2.build_dir):
            shell.call([self.toolchain.cmake, '--install', '.', '--prefix', '/', '--component', 'development'],
                       env={'DESTDIR': wasi_sysroot})

    def _build_foundation(self, swift_host_triple, has_pthread, wasi_sysroot):
        source_root = os.path.dirname(self.source_dir)
        host_toolchain = self.native_toolchain_path(self.args.host_target)

        foundation = CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(
                os.path.dirname(self.source_dir), 'swift-corelibs-foundation'),
            build_dir=os.path.join(self.build_dir, 'foundation', swift_host_triple))
        self._append_platform_cmake_options(
            foundation.cmake_options, swift_host_triple, has_pthread, wasi_sysroot, [])
        foundation.cmake_options.define('BUILD_SHARED_LIBS', 'FALSE')
        foundation.cmake_options.define('FOUNDATION_BUILD_TOOLS', 'FALSE')
        foundation.cmake_options.define('_SwiftCollections_SourceDIR', os.path.join(source_root, 'swift-collections'))
        foundation.cmake_options.define('_SwiftFoundation_SourceDIR', os.path.join(source_root, 'swift-foundation'))
        foundation.cmake_options.define('_SwiftFoundationICU_SourceDIR', os.path.join(source_root, 'swift-foundation-icu'))
        foundation.cmake_options.define('SwiftFoundation_MACRO', os.path.join(host_toolchain, 'lib', 'swift', 'host', 'plugins'))

        foundation.cmake_options.define('LIBXML2_INCLUDE_DIR', os.path.join(wasi_sysroot, 'include', 'libxml2'))
        foundation.cmake_options.define('LIBXML2_LIBRARY', os.path.join(wasi_sysroot, 'lib'))

        foundation.build_with_cmake([], self.args.build_variant, [],
                                    prefer_native_toolchain=True,
                                    ignore_extra_cmake_options=True)

        dest_dir = self._target_package_path(swift_host_triple)
        with shell.pushd(foundation.build_dir):
            shell.call([self.toolchain.cmake, '--install', '.', '--prefix', '/usr'],
                       env={'DESTDIR': dest_dir})

    def _build_swift_testing(self, swift_host_triple, has_pthread, wasi_sysroot):
        swift_testing = CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(
                os.path.dirname(self.source_dir), 'swift-testing'),
            build_dir=os.path.join(self.build_dir, 'swift-testing', swift_host_triple))
        # For statically linked objects in an archive, we have to use singlethreaded
        # LLVM codegen unit to prevent runtime metadata sections from being stripped
        # at link-time.
        self._append_platform_cmake_options(
            swift_testing.cmake_options, swift_host_triple, has_pthread, wasi_sysroot,
            extra_swift_flags=['-Xfrontend', '-enable-single-module-llvm-emission'])
        swift_testing.cmake_options.define('BUILD_SHARED_LIBS', 'FALSE')
        swift_testing.cmake_options.define(
            'CMAKE_Swift_COMPILATION_MODE', 'wholemodule')
        swift_testing.cmake_options.define('SwiftTesting_MACRO', 'NO')

        swift_testing.build_with_cmake([], self.args.build_variant, [],
                                       prefer_native_toolchain=True,
                                       ignore_extra_cmake_options=True)
        dest_dir = self._target_package_path(swift_host_triple)
        with shell.pushd(swift_testing.build_dir):
            shell.call([self.toolchain.cmake, '--install', '.', '--prefix', '/usr'],
                       env={'DESTDIR': dest_dir})

    def _build_xctest(self, swift_host_triple, has_pthread, wasi_sysroot):
        xctest = CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(
                os.path.dirname(self.source_dir), 'swift-corelibs-xctest'),
            build_dir=os.path.join(self.build_dir, 'xctest', swift_host_triple))
        self._append_platform_cmake_options(
            xctest.cmake_options, swift_host_triple, has_pthread, wasi_sysroot,
            extra_swift_flags=[])
        xctest.cmake_options.define('BUILD_SHARED_LIBS', 'FALSE')

        xctest.build_with_cmake([], self.args.build_variant, [],
                                prefer_native_toolchain=True,
                                ignore_extra_cmake_options=True)
        dest_dir = self._target_package_path(swift_host_triple)
        with shell.pushd(xctest.build_dir):
            shell.call([self.toolchain.cmake, '--install', '.', '--prefix', '/usr'],
                       env={'DESTDIR': dest_dir})

    def _build_target_package(self, swift_host_triple, has_pthread,
                              stdlib_build_path, llvm_runtime_libs_build_path,
                              wasi_sysroot):

        dest_dir = self._target_package_path(swift_host_triple)
        shell.rmtree(dest_dir)
        shell.makedirs(dest_dir)

        # Build toolchain package for standalone stdlib
        with shell.pushd(stdlib_build_path):
            shell.call([self.toolchain.cmake, '--install', '.'],
                       env={'DESTDIR': dest_dir})

        # Copy clang builtin libraries
        with shell.pushd(llvm_runtime_libs_build_path):
            for dirname in ['clang', 'swift/clang', 'swift_static/clang']:
                clang_dir = os.path.join(dest_dir, f'usr/lib/{dirname}')
                shell.call([self.toolchain.cmake, '--install', '.',
                            '--component', 'clang_rt.builtins-wasm32'],
                           env={'DESTDIR': clang_dir})

        self._build_libxml2(swift_host_triple, has_pthread, wasi_sysroot)
        self._build_foundation(swift_host_triple, has_pthread, wasi_sysroot)
        # Build swift-testing
        self._build_swift_testing(swift_host_triple, has_pthread, wasi_sysroot)
        self._build_xctest(swift_host_triple, has_pthread, wasi_sysroot)

        return dest_dir

    def build(self, host_target):
        build_root = os.path.dirname(self.build_dir)

        target_packages = []
        # NOTE: We have two types of target triples:
        # 1. swift_host_triple: The triple used by the Swift compiler's '-target' option
        # 2. clang_multiarch_triple: The triple used by Clang to find library
        #    and header paths from the sysroot
        #    https://github.com/llvm/llvm-project/blob/73ef397fcba35b7b4239c00bf3e0b4e689ca0add/clang/lib/Driver/ToolChains/WebAssembly.cpp#L29-L36
        for swift_host_triple, clang_multiarch_triple, build_basename, build_sdk, has_pthread in [
            ('wasm32-unknown-wasi', 'wasm32-wasi', 'wasmstdlib', True, False),
            # TODO: Include p1-threads in the Swift SDK once sdk-generator supports multi-target SDK
            ('wasm32-unknown-wasip1-threads', 'wasm32-wasip1-threads',
             'wasmthreadsstdlib', False, True),
        ]:
            stdlib_build_path = os.path.join(
                build_root, '%s-%s' % (build_basename, host_target))
            wasi_sysroot = wasisysroot.WASILibc.sysroot_install_path(
                build_root, clang_multiarch_triple)
            llvm_runtime_libs_build_path = os.path.join(
                build_root, '%s-%s' % ('wasmllvmruntimelibs', host_target),
                clang_multiarch_triple)
            package_path = self._build_target_package(
                swift_host_triple, has_pthread, stdlib_build_path,
                llvm_runtime_libs_build_path, wasi_sysroot)
            if build_sdk:
                target_packages.append((swift_host_triple, wasi_sysroot, package_path))

        swiftc_path = os.path.abspath(self.toolchain.swiftc)
        toolchain_path = os.path.dirname(os.path.dirname(swiftc_path))
        swift_run = os.path.join(toolchain_path, 'bin', 'swift-run')

        swift_version = os.environ.get('TOOLCHAIN_VERSION',
                                       'swift-DEVELOPMENT-SNAPSHOT')
        run_args = [
            swift_run,
            '--package-path', self.source_dir,
            '--build-path', self.build_dir,
            'swift-sdk-generator',
            'make-wasm-sdk',
            '--swift-version', swift_version,
        ]
        for swift_host_triple, wasi_sysroot, package_path in target_packages:
            run_args.extend(['--target', swift_host_triple])
            run_args.extend(['--target-swift-package-path', package_path])
            run_args.extend(['--wasi-sysroot', wasi_sysroot])

        env = dict(os.environ)
        env['SWIFTCI_USE_LOCAL_DEPS'] = '1'

        shell.call(run_args, env=env)

    def test(self, host_target):
        pass

    def should_install(self, host_target):
        return False

    @classmethod
    def get_dependencies(cls):
        return [WasmStdlib, WasmThreadsStdlib]
