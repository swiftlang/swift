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
from .swift_testing import SwiftTestingCMakeShim
from .wasmstdlib import WasmStdlib, WasmThreadsStdlib
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

    def _build_swift_testing(self, swift_host_triple, short_triple, wasi_sysroot):
        # TODO: We currently build swift-testing outside of SwiftTesting
        #       build-script product because we build Wasm stdlib outside of
        #       the main Swift build unit and we can't use build-script's cross
        #       compilation infrastructure.
        #       Once stdlib build is decoupled from compiler's CMake build unit
        #       and we can use different CMake options for different targets
        #       for stdlib build, we can fully unify library builds with the
        #       regular path.
        dest_dir = self._target_package_path(swift_host_triple)

        swift_testing = SwiftTestingCMakeShim(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(
                os.path.dirname(self.source_dir), 'swift-testing'),
            build_dir=os.path.join(
                os.path.dirname(self.build_dir),
                'swift-testing-%s' % short_triple))

        swift_testing.cmake_options.define('CMAKE_SYSTEM_NAME:STRING', 'WASI')
        swift_testing.cmake_options.define('CMAKE_SYSTEM_PROCESSOR:STRING', 'wasm32')
        swift_testing.cmake_options.define(
            'CMAKE_CXX_COMPILER_TARGET', swift_host_triple)
        swift_testing.cmake_options.define(
            'CMAKE_Swift_COMPILER_TARGET', swift_host_triple)
        swift_testing.cmake_options.define('CMAKE_SYSROOT', wasi_sysroot)
        swift_resource_dir = os.path.join(dest_dir, 'usr', 'lib', 'swift_static')
        # For statically linked objects in an archive, we have to use singlethreaded
        # LLVM codegen unit to prevent runtime metadata sections from being stripped
        # at link-time.
        swift_testing.cmake_options.define(
            'CMAKE_Swift_FLAGS',
            f'-sdk {wasi_sysroot} -resource-dir {swift_resource_dir} -Xfrontend -enable-single-module-llvm-emission')
        clang_resource_dir = os.path.join(dest_dir, 'usr', 'lib', 'clang')
        swift_testing.cmake_options.define(
            'CMAKE_CXX_FLAGS', f'-resource-dir {clang_resource_dir}')
        swift_testing.cmake_options.define('CMAKE_Swift_COMPILER_FORCED', 'TRUE')
        swift_testing.cmake_options.define('CMAKE_CXX_COMPILER_FORCED', 'TRUE')

        swift_testing.build('wasi-wasm32')
        with shell.pushd(swift_testing.build_dir):
            shell.call([self.toolchain.cmake, '--install', '.', '--prefix', '/usr'],
                       env={'DESTDIR': dest_dir})

    def _build_target_package(self, swift_host_triple, short_triple,
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
        # Build swift-testing
        self._build_swift_testing(swift_host_triple, short_triple, wasi_sysroot)

        return dest_dir

    def build(self, host_target):
        build_root = os.path.dirname(self.build_dir)
        llvm_runtime_libs_build_path = os.path.join(
            build_root, '%s-%s' % ('wasmllvmruntimelibs', host_target))

        target_packages = []
        # NOTE: We have three types of target triples:
        # 1. swift_host_triple: The triple used by the Swift compiler's '-target' option
        # 2. clang_multiarch_triple: The triple used by Clang to find library
        #    and header paths from the sysroot
        #    https://github.com/llvm/llvm-project/blob/73ef397fcba35b7b4239c00bf3e0b4e689ca0add/clang/lib/Driver/ToolChains/WebAssembly.cpp#L29-L36
        # 3. short_triple: The triple used by build-script to name the build directory
        for swift_host_triple, clang_multiarch_triple, short_triple, build_basename in [
            ('wasm32-unknown-wasi', 'wasm32-wasi', 'wasi-wasm32', 'wasmstdlib'),
            # TODO: Enable threads stdlib once sdk-generator supports multi-target SDK
            # ('wasm32-unknown-wasip1-threads', 'wasm32-wasip1-threads',
            #  'wasip1-threads-wasm32', 'wasmthreadsstdlib'),
        ]:
            stdlib_build_path = os.path.join(
                build_root, '%s-%s' % (build_basename, host_target))
            wasi_sysroot = wasisysroot.WASILibc.sysroot_install_path(
                build_root, clang_multiarch_triple)
            package_path = self._build_target_package(
                swift_host_triple, short_triple, stdlib_build_path,
                llvm_runtime_libs_build_path, wasi_sysroot)
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
