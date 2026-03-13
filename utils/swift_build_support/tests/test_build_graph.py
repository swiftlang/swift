# test_build_graph.py - Test the build_graph using mocks --------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


import unittest

from swift_build_support import build_graph


class ProductMock(object):
    def __init__(self, name):
        self.name = name
        self.deps = []

    def get_dependencies(self):
        return self.deps

    def __repr__(self):
        return "<ProductMock: {}>".format(self.name)


def get_products():
    products = {
        "cmark": ProductMock("cmark"),
        "llvm": ProductMock("llvm"),
        "swift": ProductMock("swift"),
        "swiftpm": ProductMock("swiftpm"),
        "libMockSwiftPM": ProductMock("libMockSwiftPM"),
        "libMockCMark": ProductMock("libMockCMark"),
        "libMockSwiftPM2": ProductMock("libMockSwiftPM2"),
    }

    products['llvm'].deps.extend([products['cmark']])
    products['swift'].deps.extend([products['llvm']])
    products['swiftpm'].deps.extend([products['llvm'], products['swift']])
    products['libMockSwiftPM'].deps.extend([products['swiftpm']])
    products['libMockCMark'].deps.extend([products['cmark']])
    products['libMockSwiftPM2'].deps.extend([products['swiftpm'], products['cmark']])

    return products


class BuildGraphTestCase(unittest.TestCase):

    def test_simple_build_graph(self):
        products = get_products()
        selectedProducts = [products['swiftpm']]
        schedule = build_graph.produce_scheduled_build(selectedProducts)
        names = [x.name for x in schedule[0]]
        self.assertEqual(['cmark', 'llvm', 'swift', 'swiftpm'], names)
