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
enum ArraySliceTests {
  static func main() {
    let testSuite = TestSuite("ArraySliceTests")
    testSuite.test("Identical", testIdentical)
    runAllTests()
  }

  static func testIdentical() {
    let a1: ArraySlice = [0, 1, 2, 3]
    expectTrue(a1.isTriviallyIdentical(to: a1))
    
    let a2: ArraySlice = a1
    expectTrue(a1.isTriviallyIdentical(to: a2))
    
    var a3: ArraySlice = a2
    a3.reserveCapacity(0)
    expectFalse(a1.isTriviallyIdentical(to: a3))
    
    let a4: ArraySlice = [0, 1, 2, 3]
    expectFalse(a1.isTriviallyIdentical(to: a4))
  }
}
