# swift_build_support/products/swiftpm.py -----------------------*- python -*-
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

class SwiftPM(product.Product):
    @classmethod
    def product_source_name(cls):
        return "swiftpm"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    def build(self, host_target):
        run_build_script_helper('build', host_target, self, self.args)

    def test(self, host_target):
        if self.args.test and self.args.test_swiftpm:
            run_build_script_helper('test', host_target, self, self.args)

    def install(self, host_target):
        if self.args.install_swiftpm:
            run_build_script_helper('install', host_target, self, self.args)

def run_build_script_helper(action, host_target, product, args):
    script_path = os.path.join(
        product.source_dir, 'Utilities', 'bootstrap')
    toolchain_path = args.install_destdir
    if platform.system() == 'Darwin':
        # The prefix is an absolute path, so concatenate without os.path.
        toolchain_path += \
            product.targets.darwin_toolchain_prefix(args.install_prefix)

    swiftc = os.path.join(toolchain_path, "usr", "bin", "swiftc")
    sbt = os.path.join(toolchain_path, "usr", "bin", "swift-build-tool")
    llbuild_src = os.path.join(
            os.path.dirname(product.source_dir), "llbuild")

    # FIXME: We require llbuild build directory in order to build. Is
    # there a better way to get this?
    build_root = os.path.dirname(product.build_dir)
    llbuild_build_dir = os.path.join(
            build_root, '%s-%s' % ("llbuild", host_target))

    helper_cmd = [script_path]

    if action != "build":
        helper_cmd.append(action)
    if args.build_variant != "Debug":
        helper_cmd.append("--release")

    install_prefix = args.install_destdir + args.install_prefix

    helper_cmd += [
        "--swiftc=" + swiftc,
        "--sbt=" + sbt,
        "--build", product.build_dir,
        "--llbuild-source-dir=" + llbuild_src,
        "--llbuild-build-dir=" + llbuild_build_dir,
        "--prefix", install_prefix
    ]
    shell.call(helper_cmd)
