# swift_build_support/products/tsan_libdispatch.py --------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os

from . import product
from .. import shell


def join_path(*paths):
    return os.path.abspath(os.path.join(*paths))


class TSanLibDispatch(product.Product):
    @classmethod
    def product_source_name(cls):
        return "tsan-libdispatch-test"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        """Build TSan runtime (compiler-rt)."""
        rt_source_dir = join_path(self.source_dir, os.pardir, 'compiler-rt')
        toolchain_path = join_path(self.args.install_destdir, 'usr')
        clang = join_path(toolchain_path, 'bin', 'clang')
        clangxx = join_path(toolchain_path, 'bin', 'clang++')

        config_cmd = [
            'cmake',
            '-GNinja',
            '-DCMAKE_PREFIX_PATH=%s' % toolchain_path,
            '-DCMAKE_C_COMPILER=%s' % clang,
            '-DCMAKE_CXX_COMPILER=%s' % clangxx,
            '-DCMAKE_BUILD_TYPE=Release',
            '-DLLVM_ENABLE_ASSERTIONS=ON',
            '-DCOMPILER_RT_INCLUDE_TESTS=ON',
            '-DCOMPILER_RT_BUILD_XRAY=OFF',
            '-DCOMPILER_RT_INTERCEPT_LIBDISPATCH=ON',
            '-DCOMPILER_RT_LIBDISPATCH_INSTALL_PATH=%s' % toolchain_path,
            rt_source_dir]
        build_cmd = ['ninja', 'tsan']

        # Always rebuild TSan runtime
        shell.rmtree(self.build_dir)
        shell.makedirs(self.build_dir)

        with shell.pushd(self.build_dir):
            shell.call(config_cmd)
            shell.call(build_cmd)

    def should_test(self, host_target):
        return True

    def test(self, host_target):
        """Run check-tsan target with a LIT filter for libdispatch."""
        cmd = ['ninja', 'check-tsan']
        env = {'LIT_FILTER': 'libdispatch'}

        with shell.pushd(self.build_dir):
            shell.call(cmd, env=env)

    def should_install(self, host_target):
        return False

    def install(self, host_target):
        pass
