# swift_build_support/products/foundation.py ---------------------*- python -*-
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
from . import curl
from . import libcxx
from . import libdispatch
from . import libxml2
from . import llvm
from . import product
from . import swift
from . import zlib


class Foundation(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        """is_build_script_impl_product -> bool

        Whether this product is produced by build-script-impl.
        """
        return True

    @classmethod
    def is_before_build_script_impl_product(cls):
        """is_before_build_script_impl_product -> bool

        Whether this product is built before any build-script-impl products.
        """
        return False

    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-corelibs-foundation"

    @classmethod
    def get_dependencies(cls):
        return [cmark.CMark,
                llvm.LLVM,
                libcxx.LibCXX,
                swift.Swift,
                libdispatch.LibDispatch,
                libxml2.LibXML2,
                zlib.Zlib,
                curl.LibCurl]

    @classmethod
    def is_nondarwin_only_build_product(cls):
        return True
