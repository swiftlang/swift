# swift_build_support/products/wasmkit.py ------------------------------------
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os
import shutil

from . import product
from . import swiftpm
from .. import shell


class WasmKit(product.Product):
    """
    A product for WasmKit, which is a WebAssembly runtime implementation
    written in Swift.
    """

    @classmethod
    def product_source_name(cls):
        return "wasmkit"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return True

    @classmethod
    def get_dependencies(cls):
        return [swiftpm.SwiftPM]

    def should_build(self, host_target):
        return self.args.build_wasmkit

    def should_test(self, host_target):
        return False

    def should_install(self, host_target):
        return self.args.install_wasmkit

    def install(self, host_target):
        """
        Install WasmKit to the target location
        """
        install_destdir = self.host_install_destdir(host_target)
        build_toolchain_path = install_destdir + self.args.install_prefix + '/bin'
        for swiftpm_product, dest_name in (
            ('wasmkit-cli', 'wasmkit'),
            ('wasmkit-component-ld', 'wasmkit-component-ld'),
        ):
            bin_path = run_swift_build(
                host_target, self, swiftpm_product, set_installation_rpath=True)
            shutil.copy(bin_path, os.path.join(build_toolchain_path, dest_name))

    def build(self, host_target):
        for swiftpm_product, dest in (
            ('wasmkit-cli', self.__class__.cli_file_path(self.build_dir)),
            ('wasmkit-component-ld',
             self.__class__.component_ld_file_path(self.build_dir)),
        ):
            bin_path = run_swift_build(host_target, self, swiftpm_product)
            print("Built %s at: %s" % (swiftpm_product, bin_path), flush=True)
            print("Copying %s to: %s" % (swiftpm_product, dest), flush=True)
            os.makedirs(os.path.dirname(dest), exist_ok=True)
            shutil.copy(bin_path, dest)

    @classmethod
    def cli_file_path(cls, build_dir):
        return os.path.join(build_dir, 'bin', 'wasmkit')

    @classmethod
    def component_ld_file_path(cls, build_dir):
        return os.path.join(build_dir, 'bin', 'wasmkit-component-ld')


def run_swift_build(host_target, product, swiftpm_package_product_name, set_installation_rpath=False):
    swift_build = os.path.join(product.install_toolchain_path(host_target), "bin", "swift-build")

    if not os.path.exists(swift_build) or product.args.build_runtime_with_host_compiler:
        print(
            f"WARNING: build-script's {os.path.basename(__file__)} is running local development code path, "
            "don't use these build artifacts for deployment!"
        )
        swift_build = product.toolchain.swift_build

    if host_target.startswith('macos'):
        # Universal binary on macOS
        platform_args = ['--arch', 'x86_64', '--arch', 'arm64']
    elif set_installation_rpath:
        # Library rpath for swift, dispatch, Foundation, etc. when installing
        build_os = host_target.split('-')[0]
        platform_args = [
            '--disable-local-rpath', '-Xswiftc', '-no-toolchain-stdlib-rpath',
            '-Xlinker', '-rpath', '-Xlinker', '$ORIGIN/../lib/swift/' + build_os
        ]
    else:
        platform_args = []

    build_args = [
        swift_build,
        '--product', swiftpm_package_product_name,
        '--package-path', os.path.join(product.source_dir),
        '--build-path', product.build_dir,
        '--configuration', 'release',
        '--traits', 'ComponentModel,FileSystem,WasmDebuggingSupport',
    ] + platform_args

    if product.args.verbose_build:
        build_args.append('--verbose')

    env = dict(os.environ)
    env['SWIFTCI_USE_LOCAL_DEPS'] = '1'

    shell.call(build_args, env=env)

    bin_dir_path = shell.capture(
        build_args + ['--show-bin-path'], dry_run=False, echo=False).rstrip()
    return os.path.join(bin_dir_path, swiftpm_package_product_name)
