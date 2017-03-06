# swift_build_support/products/product.py -----------------------*- python -*-
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


class Product(object):
    @classmethod
    def product_name(cls):
        """product_name() -> str

        The identifier-style name to use for this product.
        """
        return cls.__name__.lower()

    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        It provides a customization point for Product subclasses. It is set to
        the value of product_name() by default for this reason.
        """
        return cls.product_name()

    @classmethod
    def get_build_directory_name(cls, host_target):
        return "{}-{}".format(cls.product_name(),
                              host_target.name)

    def __init__(self, args, toolchain, source_dir, build_dir):
        self.args = args
        self.toolchain = toolchain
        self.source_dir = source_dir
        self.build_dir = build_dir
        self.cmake_options = []
