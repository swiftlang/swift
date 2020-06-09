# swift_build_support/products/swiftevolve.py --------------------*- python -*-
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
from . import libcxx
from . import libdispatch
from . import libicu
from . import llbuild
from . import llvm
from . import skstresstester
from . import swift
from . import swiftpm
from . import swiftsyntax
from . import xctest


class SwiftEvolve(skstresstester.SKStressTester):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-stress-tester"

    def package_name(self):
        return 'SwiftEvolve'

    # Inherit the entire build configuration from the SourceKit stress tester

    def should_build(self, host_target):
        return True

    def should_test(self, host_target):
        return self.args.test_swiftevolve

    def should_install(self, host_target):
        return self.args.install_swiftevolve

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
                llbuild.LLBuild,
                swiftpm.SwiftPM,
                swiftsyntax.SwiftSyntax,
                skstresstester.SKStressTester]
