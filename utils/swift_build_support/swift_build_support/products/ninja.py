# swift_build_support/products/ninja.py -------------------------*- python -*-
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
"""
Ninja build
"""
# ----------------------------------------------------------------------------

import os.path

from build_swift.build_swift import cache_utils

from . import product
from .. import shell


class Ninja(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def new_builder(cls, args, toolchain, workspace, host):
        return NinjaBuilder(cls, args, toolchain, workspace)


class NinjaBuilder(product.ProductBuilder):
    def __init__(self, product_class, args, toolchain, workspace):
        self.source_dir = workspace.source_dir(
            product_class.product_source_name())
        self.build_dir = workspace.build_dir('build',
                                             product_class.product_name())
        self.args = args
        self.toolchain = toolchain

    @cache_utils.reify
    def ninja_bin_path(self):
        return os.path.join(self.build_dir, 'ninja')

    def build(self):
        if os.path.exists(self.ninja_bin_path):
            return
        shell.call([
            self.toolchain.cmake,
            "-S", self.source_dir,
            "-B", self.build_dir,
            "-DCMAKE_BUILD_TYPE=Release",
            f"-DCMAKE_C_COMPILER={self.toolchain.cc}",
            f"-DCMAKE_CXX_COMPILER={self.toolchain.cxx}"])
        shell.call([self.toolchain.cmake, "--build", self.build_dir])
