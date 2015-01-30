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
// RUN: %target-run-simple-swift
//
// XFAIL: interpret
// XFAIL: linux

import StdlibUnittest

var LazyTestSuite = TestSuite("Lazy")

LazyTestSuite.test("isEmpty") {
  expectTrue(lazy(0..<0).isEmpty)
  expectFalse(lazy(0...0).isEmpty)
}

LazyTestSuite.test("firstLast") {
  expectOptionalEqual(7, lazy(7..<42).first)
  expectOptionalEqual(41, lazy(7..<42).last)
  expectEmpty(lazy(7..<7).first)
  expectEmpty(lazy(7..<7).last)
}

runAllTests()

