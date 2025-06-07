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
        # Currently, it's only used for testing stdlib.
        return True

    def install(self, host_target):
        """
        Install WasmKit to the target location
        """
        install_destdir = self.host_install_destdir(host_target)
        build_toolchain_path = install_destdir + self.args.install_prefix + '/bin'
        bin_path = run_swift_build(host_target, self, 'wasmkit-cli', set_installation_rpath=True)
        shutil.copy(bin_path, build_toolchain_path + '/wasmkit')

    def build(self, host_target):
        bin_path = run_swift_build(host_target, self, 'wasmkit-cli')
        print("Built wasmkit-cli at: " + bin_path)
        # Copy the built binary to ./bin
        dest_bin_path = self.__class__.cli_file_path(self.build_dir)
        print("Copying wasmkit-cli to: " + dest_bin_path)
        os.makedirs(os.path.dirname(dest_bin_path), exist_ok=True)
        shutil.copy(bin_path, dest_bin_path)

    @classmethod
    def cli_file_path(cls, build_dir):
        return os.path.join(build_dir, 'bin', 'wasmkit-cli')


def run_swift_build(host_target, product, swiftpm_package_product_name, set_installation_rpath=False):
    # Building with the freshly-built SwiftPM
    swift_build = os.path.join(product.install_toolchain_path(host_target), "bin", "swift-build")

    build_os = host_target.split('-')[0]
    if set_installation_rpath and not host_target.startswith('macos'):
        # Library rpath for swift, dispatch, Foundation, etc. when installing
        rpath_args = [
            '--disable-local-rpath', '-Xswiftc', '-no-toolchain-stdlib-rpath',
            '-Xlinker', '-rpath', '-Xlinker', '$ORIGIN/../lib/swift/' + build_os
        ]
    else:
        rpath_args = []

    build_args = [
        swift_build,
        '--product', swiftpm_package_product_name,
        '--package-path', os.path.join(product.source_dir),
        '--build-path', product.build_dir,
        '--configuration', 'release',
    ] + rpath_args

    if product.args.verbose_build:
        build_args.append('--verbose')

    env = dict(os.environ)
    env['SWIFTCI_USE_LOCAL_DEPS'] = '1'

    shell.call(build_args, env=env)

    bin_dir_path = shell.capture(
        build_args + ['--show-bin-path'], dry_run=False, echo=False).rstrip()
    return os.path.join(bin_dir_path, swiftpm_package_product_name)
