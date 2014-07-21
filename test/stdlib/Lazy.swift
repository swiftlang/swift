//===--- Lazy.swift - Tests for LazySequence and LazyCollection -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift | FileCheck %s
import StdlibUnittest

var LazyTestCase = TestCase("Lazy")

LazyTestCase.test("isEmpty") {
  expectTrue(lazy(0..<0).isEmpty)
  expectFalse(lazy(0...0).isEmpty)
}

LazyTestCase.test("firstLast") {
  expectEqual(7, lazy(7..<42).first())
  expectEqual(41, lazy(7..<42).last())
}

LazyTestCase.run()
// CHECK: {{^}}Lazy: All tests passed

