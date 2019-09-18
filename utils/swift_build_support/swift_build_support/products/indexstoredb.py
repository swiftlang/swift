# swift_build_support/products/indexstoredb.py -------------------*- python -*-
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

import os
import platform

from . import product
from .. import shell
from .. import targets


class IndexStoreDB(product.Product):
    @classmethod
    def product_source_name(cls):
        return "indexstore-db"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    def build(self, host_target):
        run_build_script_helper('build', host_target, self, self.args)

    def test(self, host_target):
        if self.args.test and self.args.test_indexstoredb:
            run_build_script_helper('test', host_target, self, self.args)

    def install(self, host_target):
        pass


def run_build_script_helper(action, host_target, product, args):
    script_path = os.path.join(
        product.source_dir, 'Utilities', 'build-script-helper.py')
    toolchain_path = args.install_destdir
    if platform.system() == 'Darwin':
        # The prefix is an absolute path, so concatenate without os.path.
        toolchain_path += \
            targets.darwin_toolchain_prefix(args.install_prefix)
    configuration = 'debug' if args.build_variant == 'Debug' else 'release'
    helper_cmd = [
        script_path,
        action,
        '--verbose',
        '--package-path', product.source_dir,
        '--build-path', product.build_dir,
        '--configuration', configuration,
        '--toolchain', toolchain_path,
        '--ninja-bin', product.toolchain.ninja,
    ]
    shell.call(helper_cmd)
