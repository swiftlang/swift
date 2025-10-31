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
enum SetTests {
  static func main() {
    let testSuite = TestSuite("SetTests")
    testSuite.test("Identical", testIdentical)
    runAllTests()
  }

  static func testIdentical() {
    let s1: Set = [0, 1, 2, 3]
    expectTrue(s1.isTriviallyIdentical(to: s1))

    let s2: Set = s1
    expectTrue(s1.isTriviallyIdentical(to: s2))

    var s3: Set = s2
    s3.reserveCapacity(0)
    expectFalse(s1.isTriviallyIdentical(to: s3))

    let s4: Set = [0, 1, 2, 3]
    expectFalse(s1.isTriviallyIdentical(to: s4))
  }
}
