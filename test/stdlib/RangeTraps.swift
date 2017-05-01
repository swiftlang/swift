//===--- RangeTraps.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug
// RUN: %target-run %t/a.out_Release
// REQUIRES: executable_test
// CountablePartialRangeFrom fails in resilient mode. <rdar://problem/31909976>
// XFAIL: resilient_stdlib


import StdlibUnittest

let testSuiteSuffix = _isDebugAssertConfiguration() ? "_debug" : "_release"

var RangeTraps = TestSuite("RangeTraps" + testSuiteSuffix)

RangeTraps.test("HalfOpen")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var range = 1..<1
  expectType(CountableRange<Int>.self, &range)
  
  expectCrashLater()
  _ = 1..<0
}

RangeTraps.test("Closed")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  var range = 1...1
  expectType(CountableClosedRange<Int>.self, &range)

  expectCrashLater()
  _ = 1...0
}

RangeTraps.test("OutOfRange")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
  _ = 0..<Int.max // This is a CountableRange

  // This works for Ranges now!
  expectTrue((0...Int.max).contains(Int.max))
}

RangeTraps.test("CountablePartialRangeFrom")
  .skip(.custom(
    { _isFastAssertConfiguration() },
    reason: "this trap is not guaranteed to happen in -Ounchecked"))
  .code {
    
    let range = (Int.max - 1)...
    var it = range.makeIterator()
    _ = it.next()
    expectCrashLater()
    _ = it.next()
}



runAllTests()

