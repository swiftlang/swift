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

from . import cmark
from . import foundation
from . import indexstoredb
from . import libcxx
from . import libdispatch
from . import libicu
from . import llbuild
from . import llvm
from . import product
from . import swift
from . import xctest


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
        indexstoredb.run_build_script_helper(
            'clean', host_target, self, self.args)

    def build(self, host_target):
        indexstoredb.run_build_script_helper(
            'build', host_target, self, self.args)

    def should_test(self, host_target):
        return self.args.test_swift_driver

    def test(self, host_target):
        indexstoredb.run_build_script_helper(
            'test', host_target, self, self.args,
            self.args.test_sourcekitlsp_sanitize_all)

    def should_install(self, host_target):
        return self.args.install_swift_driver

    def install(self, host_target):
        indexstoredb.run_build_script_helper(
            'install', host_target, self, self.args)
