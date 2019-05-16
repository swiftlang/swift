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

from . import product
from .build_script_helper_builder import BuildScriptHelperBuilder


class SourceKitLSP(product.Product):
    @classmethod
    def product_source_name(cls):
        return "sourcekit-lsp"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def new_builder(cls, args, toolchain, workspace, host):
        return SourceKitLSPBuilder(cls, args, toolchain, workspace, host)


class SourceKitLSPBuilder(BuildScriptHelperBuilder):
    def __init__(self, product_class, args, toolchain, workspace, host):
        BuildScriptHelperBuilder.__init__(self, product_class, args, toolchain,
                                          workspace, host)
        self.__args = args

    def _should_test(self):
        return self.__args.test and self.__args.test_sourcekitlsp
