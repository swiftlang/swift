//===----------------------------------------------------------------------===//
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
//
// RUN: %target-run-simple-swift(-parse-as-library)
// REQUIRES: executable_test
// END.
//
//===----------------------------------------------------------------------===//

import StdlibUnittest

@main
enum DictionaryTests {
  static func main() {
    let testSuite = TestSuite("DictionaryTests")
    testSuite.test("Identical", testIdentical)
    runAllTests()
  }

  static func testIdentical() {
    let d1: Dictionary = ["a": 1, "b": 2, "c": 3]
    expectTrue(d1.isTriviallyIdentical(to: d1))

    let d2: Dictionary = d1
    expectTrue(d1.isTriviallyIdentical(to: d2))

    var d3: Dictionary = d2
    d3.reserveCapacity(0)
    expectFalse(d1.isTriviallyIdentical(to: d3))

    let d4: Dictionary = ["a": 1, "b": 2, "c": 3]
    expectFalse(d1.isTriviallyIdentical(to: d4))
  }
}
