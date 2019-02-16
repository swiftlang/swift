# swift_build_support/products/libcxx.py -------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

from . import product
from . import llvm

import os.path

class LibCXX(product.Product):

    @classmethod
    def builder_class(cls):
        return LibCXXBuilder


class LibCXXBuilder(llvm.LLVMBase):
    def __init__(self, product_class, args, toolchain, workspace, host):
        product.CMakeProductBuilder.__init__(self, product_class, args,
                                             toolchain, workspace, host)
        self._cmake_options.define('CMAKE_C_FLAGS_RELWITHDEBINFO', '-O2 -DNDEBUG')
        self._cmake_options.define('CMAKE_CXX_FLAGS_RELWITHDEBINFO', '-O2 -DNDEBUG')
        self._cmake_options.define('CMAKE_BUILD_TYPE:STRING', self._build_variant)
        self._cmake_options.define('LLVM_INCLUDE_DOCS:BOOL', True)
        self._cmake_options.define('LLVM_CONFIG_PATH', os.path.join(self._workspace.build_dir(self._args.host_target, 'llvm'), 'bin', 'llvm-config'))
        self._cmake_options += self._llvm_cmake_options

    @property
    def _build_targets(self):
        return ['cxx-headers']

    @property
    def _build_variant(self):
        return self._args.llvm_build_variant

    @property
    def _should_test(self):
        return False # We don't test libc++
