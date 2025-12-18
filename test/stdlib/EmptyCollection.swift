//===--- EmptyCollection.swift ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var testSuite = TestSuite("EmptyCollection")

struct NonHashable {}

if #available(SwiftStdlib 6.3, *) {
  testSuite.test("UnconditionallyHashable") {
    checkHashable(expectedEqual: true, EmptyCollection<AnyHashable>(), EmptyCollection<AnyHashable>())
    checkHashable(expectedEqual: true, EmptyCollection<NonHashable>(), EmptyCollection<NonHashable>())
  }
}

runAllTests()

