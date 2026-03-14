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
import re

from build_swift.build_swift import cache_utils

from . import product
from .. import shell
from ..utils import log_time_in_scope


class Ninja(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def new_builder(cls, args, toolchain, workspace):
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

        print("--- Local Ninja Build ---")
        with log_time_in_scope('local ninja'):
            shell.call([
                self.toolchain.cmake,
                "-S", self.source_dir,
                "-B", self.build_dir,
                "-DCMAKE_BUILD_TYPE=Release",
                "-DBUILD_TESTING=OFF",
                f"-DCMAKE_C_COMPILER={self.toolchain.cc}",
                f"-DCMAKE_CXX_COMPILER={self.toolchain.cxx}"])
            shell.call([self.toolchain.cmake, "--build", self.build_dir])


def get_ninja_version(ninja_bin_path):
    if not ninja_bin_path or not os.path.isfile(ninja_bin_path):
        return
    ninja_version_pattern = re.compile(r'^(\d+)\.(\d+)\.(\d+).*')
    version = shell.capture([ninja_bin_path, "--version"], dry_run=False,
                            echo=True, optional=True)
    m = ninja_version_pattern.match(version)
    if m is None:
        return
    (major, minor, patch) = map(int, m.groups())
    return (major, minor, patch)


def get_ninja_path(toolchain, args, workspace):
    min_ninja_version = (1, 8, 2)

    built_ninja_path = os.path.join(workspace.build_dir('build', 'ninja'), 'ninja')
    built_ninja_version = get_ninja_version(built_ninja_path)
    if built_ninja_version and min_ninja_version <= built_ninja_version:
        return built_ninja_path

    toolchain_ninja_version = get_ninja_version(toolchain.ninja)
    if toolchain_ninja_version and min_ninja_version <= toolchain_ninja_version:
        return toolchain.ninja

    # Build ninja from source
    ninja_build = Ninja.new_builder(args=args,
                                    toolchain=toolchain,
                                    workspace=workspace)
    ninja_build.build()
    return ninja_build.ninja_bin_path
