# swift_build_support/products/llvm.py --------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

from . import product


class LLVM(product.Product):

    def __init__(self, args, toolchain, source_dir, build_dir):
        product.Product.__init__(self, args, toolchain, source_dir,
                                 build_dir)

        # Add the cmake option for enabling or disabling assertions.
        self.cmake_options.extend([
            '-DLLVM_ENABLE_ASSERTIONS=%s' % str(args.llvm_assertions).upper()
        ])

        # Add the cmake option for LLVM_TARGETS_TO_BUILD.
        self.cmake_options.extend([
            '-DLLVM_TARGETS_TO_BUILD=%s' % args.llvm_targets_to_build
        ])
