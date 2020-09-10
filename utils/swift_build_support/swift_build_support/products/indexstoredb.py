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

from . import product
from . import swiftpm
from .. import shell
from .. import targets


class IndexStoreDB(product.Product):
    @classmethod
    def product_source_name(cls):
        return "indexstore-db"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        run_build_script_helper('build', host_target, self, self.args)

    def should_test(self, host_target):
        return self.args.test_indexstoredb

    def test(self, host_target):
        run_build_script_helper('test', host_target, self, self.args,
                                self.args.test_indexstoredb_sanitize_all)

    def should_install(self, host_target):
        return False

    def install(self, host_target):
        pass


def run_build_script_helper(action, host_target, product, args,
                            sanitize_all=False):
    script_path = os.path.join(
        product.source_dir, 'Utilities', 'build-script-helper.py')

    install_destdir = args.install_destdir
    if swiftpm.SwiftPM.has_cross_compile_hosts(args):
        install_destdir = swiftpm.SwiftPM.get_install_destdir(args,
                                                              host_target,
                                                              product.build_dir)
    toolchain_path = targets.toolchain_path(install_destdir,
                                            args.install_prefix)
    is_release = product.is_release()
    configuration = 'release' if is_release else 'debug'
    helper_cmd = [
        script_path,
        action,
        '--package-path', product.source_dir,
        '--build-path', product.build_dir,
        '--configuration', configuration,
        '--toolchain', toolchain_path,
        '--ninja-bin', product.toolchain.ninja,
    ]
    if args.verbose_build:
        helper_cmd.append('--verbose')

    if sanitize_all:
        helper_cmd.append('--sanitize-all')
    elif args.enable_asan:
        helper_cmd.extend(['--sanitize', 'address'])
    elif args.enable_ubsan:
        helper_cmd.extend(['--sanitize', 'undefined'])
    elif args.enable_tsan:
        helper_cmd.extend(['--sanitize', 'thread'])

    shell.call(helper_cmd)
