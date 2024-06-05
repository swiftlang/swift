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

    def _target_package_path(self, target_triple):
        return os.path.join(self.build_dir, 'Toolchains', target_triple)

    def _build_target_package(self, target_triple,
                              stdlib_build_path, llvm_runtime_libs_build_path):

        dest_dir = self._target_package_path(target_triple)
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

        return dest_dir

    def build(self, host_target):
        build_root = os.path.dirname(self.build_dir)
        llvm_runtime_libs_build_path = os.path.join(
            build_root, '%s-%s' % ('wasmllvmruntimelibs', host_target))

        target_packages = []
        for target_triple, short_triple, build_basename in [
            ('wasm32-unknown-wasi', 'wasm32-wasi', 'wasmstdlib'),
            # TODO: Enable threads stdlib once sdk-generator supports multi-target SDK
            # ('wasm32-unknown-wasip1-threads', 'wasmthreadsstdlib'),
        ]:
            stdlib_build_path = os.path.join(
                build_root, '%s-%s' % (build_basename, host_target))
            package_path = self._build_target_package(
                target_triple, stdlib_build_path, llvm_runtime_libs_build_path)
            target_packages.append((target_triple, package_path))

        swiftc_path = os.path.abspath(self.toolchain.swiftc)
        toolchain_path = os.path.dirname(os.path.dirname(swiftc_path))
        swift_run = os.path.join(toolchain_path, 'bin', 'swift-run')

        swift_version = os.environ.get('TOOLCHAIN_VERSION',
                                       'swift-DEVELOPMENT-SNAPSHOT').lstrip('swift-')
        run_args = [
            swift_run,
            '--package-path', self.source_dir,
            '--build-path', self.build_dir,
            'swift-sdk-generator',
            'make-wasm-sdk',
            '--swift-version', swift_version,
        ]
        for target_triple, package_path in target_packages:
            run_args.extend(['--target', target_triple])
            run_args.extend(['--target-swift-package-path', package_path])
            wasi_sysroot = wasisysroot.WASILibc.sysroot_install_path(
                build_root, short_triple)
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
