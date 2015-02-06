//===--- IntervalTraps.swift ----------------------------------------------===//
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
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out_Debug
// RUN: %target-build-swift %s -o %t/a.out_Release -O
//
// RUN: %target-run %t/a.out_Debug
// RUN: %target-run %t/a.out_Release

// XFAIL: linux

import StdlibUnittest

var IntervalTraps = TestSuite("IntervalTraps")

IntervalTraps.test("HalfOpen") {
  var interval = 1.0..<1.0
  expectType(HalfOpenInterval<Double>.self, &interval)

  expectCrashLater()
  1.0..<0.0
}

IntervalTraps.test("Closed") {
  var interval = 1.0...1.0
  expectType(ClosedInterval<Double>.self, &interval)

  expectCrashLater()
  1.0...0.0
}

runAllTests()

