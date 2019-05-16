# swift_build_support/products/benchmarks.py --------------------*- python -*-
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
import platform

from . import product
from .. import shell
from .. import targets


# Build against the current installed toolchain.
class Benchmarks(product.Product):
    @classmethod
    def product_source_name(cls):
        return "benchmarks"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def new_builder(cls, args, toolchain, workspace, host):
        return BenchmarksBuilder(cls, args, toolchain, workspace, host)


# NOTE: this is very similar to BuildScriptHelperBuilder, but also
# different enough to justify a different builder altogether.
class BenchmarksBuilder(product.ProductBuilder):
    def __init__(self, product_class, args, toolchain, workspace, host):
        # Our source directory is a subdirectory inside the swift source
        # directory
        self.__source_dir = os.path.join(workspace.source_dir('swift'),
                                         'benchmark')
        self.__build_dir = workspace.build_dir(host.name,
                                               product_class.product_name())
        self.__args = args

    def build(self):
        # We use a separate python helper to enable quicker iteration when
        # working on this by avoiding going through build-script to test small
        # changes.
        script_path = os.path.join(
            self.__source_dir, 'scripts', 'build_script_helper.py')
        toolchain_path = self.__args.install_destdir
        if platform.system() == 'Darwin':
            # The prefix is an absolute path, so concatenate without os.path.
            toolchain_path += \
                targets.darwin_toolchain_prefix(self.__args.install_prefix)
        helper_cmd = [
            script_path,
            '--verbose',
            '--package-path', self.__source_dir,
            '--build-path', self.__build_dir,
            '--toolchain', toolchain_path,
        ]
        shell.call(helper_cmd)

    def test(self):
        """Just run a single instance of the command for both .debug and
           .release.
        """
        cmdline = ['--num-iters=1', 'XorLoop']
        bench_Onone = os.path.join(self.__build_dir, 'bin', 'Benchmark_Onone')
        shell.call([bench_Onone] + cmdline)

        bench_O = os.path.join(self.__build_dir, 'bin', 'Benchmark_O')
        shell.call([bench_O] + cmdline)

        bench_Osize = os.path.join(self.__build_dir, 'bin', 'Benchmark_Osize')
        shell.call([bench_Osize] + cmdline)
