# swift_build_support/products/swiftdriver.py -------------------*- python -*-
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

from . import cmark
from . import foundation
from . import libcxx
from . import libdispatch
from . import libicu
from . import llbuild
from . import llvm
from . import product
from . import swift
from . import swiftpm
from . import xctest
from .. import shell
from .. import targets


class SwiftDriver(product.Product):
    @classmethod
    def product_source_name(cls):
        return "swift-driver"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return self.args.build_swift_driver

    @classmethod
    def get_dependencies(cls):
        return [cmark.CMark,
                llvm.LLVM,
                libcxx.LibCXX,
                libicu.LibICU,
                swift.Swift,
                libdispatch.LibDispatch,
                foundation.Foundation,
                xctest.XCTest,
                llbuild.LLBuild]

    def should_clean(self, host_target):
        return self.args.clean_swift_driver

    def clean(self, host_target):
        run_build_script_helper('clean', host_target, self, self.args)

    def build(self, host_target):
        run_build_script_helper('build', host_target, self, self.args)

    def should_test(self, host_target):
        return self.args.test_swift_driver

    def test(self, host_target):
        run_build_script_helper('test', host_target, self, self.args)

    def should_install(self, host_target):
        return self.args.install_swift_driver

    def install(self, host_target):
        run_build_script_helper('install', host_target, self, self.args)


def run_build_script_helper(action, host_target, product, args):
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
        '--cmake-bin', product.toolchain.cmake,
    ]
    if args.verbose_build:
        helper_cmd.append('--verbose')

    shell.call(helper_cmd)
