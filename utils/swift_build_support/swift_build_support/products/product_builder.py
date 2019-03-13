# swift_build_support/product_builders/product_builder.py -------*- python -*-
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

import abc


class ProductBuilder(object):
    """
    Abstract base class for all ProductBuilders.

    An specific ProductBuilder will implement the interface methods depending
    how the product want to be build. Multiple products can use the same
    product builder if parametrized right (for example all the products build
    using CMake).

    Ideally a ProductBuilder will be initialized with references to the
    invocation arguments, the calculated toolchain, the calculated workspace,
    and the target host, but the base class doesn't impose those requirements
    in order to be flexible.

    NOTE: Python doesn't need an explicit abstract base class, but it helps
    documenting the interface.
    """

    @abc.abstractmethod
    def do_build(self):
        """
        Perform the build phase for the product.

        This phase might also imply a configuration phase, but each product
        builder is free to determine how to do it.
        """
        pass

    @abc.abstractmethod
    def do_test(self):
        """
        Perform the test phase for the product.

        This phase might build and execute the product tests.
        """
        pass

    @abc.abstractmethod
    def do_install(self):
        """
        Perform the install phase for the product.

        This phase might copy the artifacts from the previous phases into a
        destination directory.
        """
        pass
