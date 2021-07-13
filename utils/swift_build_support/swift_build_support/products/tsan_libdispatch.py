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

from . import cmake_product
from . import foundation
from . import libcxx
from . import libdispatch
from . import libicu
from . import llvm
from .. import shell


def join_path(*paths):
    return os.path.abspath(os.path.join(*paths))


class TSanLibDispatch(cmake_product.CMakeProduct):
    @classmethod
    def product_source_name(cls):
        return "tsan-libdispatch-test"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        """Build TSan runtime (compiler-rt)."""
        # Fixup source dir for CMake: <root>/tsan-libdispatch-test -> <root>/llvm-project/compiler-rt
        self.source_dir = join_path(self.source_dir, os.pardir, 'llvm-project', 'compiler-rt')

        toolchain_path = join_path(self.args.install_destdir, 'usr')
        clang = join_path(toolchain_path, 'bin', 'clang')
        clangxx = join_path(toolchain_path, 'bin', 'clang++')

        self.cmake_options.define('CMAKE_PREFIX_PATH', toolchain_path)
        self.cmake_options.define('CMAKE_C_COMPILER', clang)
        self.cmake_options.define('CMAKE_CXX_COMPILER', clangxx)
        self.cmake_options.define('CMAKE_BUILD_TYPE', 'Release')
        self.cmake_options.define('LLVM_ENABLE_ASSERTIONS', 'ON')
        self.cmake_options.define('COMPILER_RT_DEBUG', 'ON')
        self.cmake_options.define('COMPILER_RT_INCLUDE_TESTS', 'ON')
        self.cmake_options.define('COMPILER_RT_BUILD_XRAY', 'OFF')
        self.cmake_options.define('COMPILER_RT_INTERCEPT_LIBDISPATCH', 'ON')
        self.cmake_options.define('COMPILER_RT_LIBDISPATCH_INSTALL_PATH', toolchain_path)

        self.build_with_cmake(['tsan'], 'Release', [])

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

    @classmethod
    def get_dependencies(cls):
        return [llvm.LLVM,
                libcxx.LibCXX,
                libicu.LibICU,
                libdispatch.LibDispatch,
                foundation.Foundation]
