//===--- ArrayBuffer_CopyContents.swift -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_stdlib_asserts
// REQUIRES: foundation

import Foundation
import StdlibUnittest

let suite = TestSuite("ArrayBuffer_CopyContents")
defer { runAllTests() }


var trackedCount = 0
var nextBaseSerialNumber = 0

/// A type that will be bridged verbatim to Objective-C
class Thing: NSObject {
  var value: Int
  var serialNumber: Int

  func foo() { }

  required init(_ value: Int) {
    trackedCount += 1
    nextBaseSerialNumber += 1
    serialNumber = nextBaseSerialNumber
    self.value = value
  }

  deinit {
    assert(serialNumber > 0, "double destruction!")
    trackedCount -= 1
    serialNumber = -serialNumber
  }

  override func isEqual(_ other: Any?) -> Bool {
    return (other as? Thing)?.value == self.value
  }

  override var hash: Int { value }
}


suite.test("nativeArray/_copyContents") {
  let array = [Thing(0), Thing(1), Thing(2), Thing(3)]
  expectEqualSequence(array._copyToNewArray(), array)
}

suite.test("nativeArraySlice/_copyContents") {
  let array = (0 ..< 100).map { Thing($0) }
  expectEqualSequence(
    array[20 ..< 30]._copyToNewArray(),
    (20 ..< 30).map { Thing($0) })
}

suite.test("bridgedArray/_copyContents") {
  let array = NSArray(array: (0 ..< 5).map { Thing($0) }) as! [Thing]
  expectEqualSequence(
    array._copyToNewArray(),
    (0 ..< 5).map { Thing($0) })
}

suite.test("bridgedArraySlice/_copyContents") {
  let array = NSArray(array: (0 ..< 100).map { Thing($0) }) as! [Thing]
  expectEqualSequence(
    array[20 ..< 30]._copyToNewArray(),
    (20 ..< 30).map { Thing($0) })
}
