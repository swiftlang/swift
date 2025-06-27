# swift_build_support/products/staticswiftlinux.py --------------*- python -*-
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

from . import earlyswiftdriver
from . import product


class StaticSwiftLinuxConfig(product.Product):

    @classmethod
    def is_build_script_impl_product(cls):
        """is_build_script_impl_product -> bool

        Whether this product is produced by build-script-impl.
        """
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        """is_before_build_script_impl_product -> bool

        Whether this product is built before any build-script-impl products.
        Such products must be non-build_script_impl products.
        Because such products are built ahead of the compiler, they are
        built using the host toolchain.
        """
        return False

    @classmethod
    def get_dependencies(cls):
        """Return a list of products that this product depends upon"""
        # This is a lie; we don't depend on EarlySwiftDriver, but the
        # DAG generator used for --infer doesn't support more than one
        # root.
        return [earlyswiftdriver.EarlySwiftDriver]

    def install_static_linux_config(self, arch, bin_dir):
        """Install the .cfg files to set the relevant Clang options for the
        fully static Linux SDK's <arch>-swift-linux-musl triple.

        Doing it this way means it's easier to modify the defaults without
        having to change the compiler driver."""

        try:
            os.makedirs(bin_dir)
        except FileExistsError:
            pass

        musl_cfg_name = f'{arch}-swift-linux-musl-clang.cfg'
        musl_cfg = os.path.join(bin_dir, musl_cfg_name)
        with open(musl_cfg, "wt") as f:
            f.write(f"""
-target {arch}-swift-linux-musl
-rtlib=compiler-rt
-stdlib=libc++
-fuse-ld=lld
-unwindlib=libunwind
-lc++abi
-static
            """)
        for name in (f'{arch}-swift-linux-musl-clang++.cfg', ):
            try:
                os.symlink(musl_cfg_name, os.path.join(bin_dir, name))
            except FileExistsError:
                pass

    def clean(self, host_target):
        """clean() -> void

        Perform the clean, for a non-build-script-impl product.
        """
        pass

    def should_build(self, host_target):
        """should_build() -> Bool

        Whether or not this product should be built with the given arguments.
        """
        return False

    async def build(self, host_target):
        """build() -> void

        Perform the build, for a non-build-script-impl product.
        """
        pass

    def should_test(self, host_target):
        """should_test() -> Bool

        Whether or not this product should be tested with the given arguments.
        """
        return False

    def test(self, host_target):
        """test() -> void

        Run the tests, for a non-build-script-impl product.
        """
        pass

    def should_install(self, host_target):
        """should_install() -> Bool

        Whether or not this product should be installed with the given
        arguments.
        """
        return self.args.install_static_linux_config

    def install(self, host_target):
        """install() -> void

        Install to the toolchain, for a non-build-script-impl product.
        """
        host_install_destdir = self.host_install_destdir(host_target)

        clang_dest_dir = '{}{}'.format(host_install_destdir,
                                       self.args.install_prefix)

        bin_dir = os.path.join(clang_dest_dir, 'bin')
        for arch in self.args.linux_static_archs:
            self.install_static_linux_config(arch, bin_dir)
