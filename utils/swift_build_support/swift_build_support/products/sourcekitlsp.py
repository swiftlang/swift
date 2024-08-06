# swift_build_support/products/sourcekitlsp.py -------------------*- python -*-
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
from . import llbuild
from . import llvm
from . import product
from . import swift
from . import swiftpm
from . import xctest


class SourceKitLSP(product.Product):
    @classmethod
    def product_source_name(cls):
        return "sourcekit-lsp"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def is_swiftpm_unified_build_product(cls):
        return True

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        indexstoredb.run_build_script_helper(
            'build', host_target, self, self.args)

    def should_test(self, host_target):
        return self.args.test_sourcekitlsp

    def test(self, host_target):
        indexstoredb.run_build_script_helper(
            'test', host_target, self, self.args,
            self.args.test_sourcekitlsp_sanitize_all)

    def should_install(self, host_target):
        return self.args.install_sourcekitlsp

    def install(self, host_target):
        indexstoredb.run_build_script_helper(
            'install', host_target, self, self.args)

    @classmethod
    def get_dependencies(cls):
        return [cmark.CMark,
                llvm.LLVM,
                libcxx.LibCXX,
                swift.Swift,
                libdispatch.LibDispatch,
                foundation.Foundation,
                xctest.XCTest,
                llbuild.LLBuild,
                swiftpm.SwiftPM]
