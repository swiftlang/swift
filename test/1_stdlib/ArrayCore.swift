//===--- ArrayCore.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import Swift

//===--- class Tracked ----------------------------------------------------===//
// Instead of testing with Int elements, we use this wrapper class
// that can help us track allocations and find issues with object
// lifetime inside Array implementations.
var trackedCount = 0
var nextTrackedSerialNumber = 0

final class Tracked : ForwardIndex, CustomStringConvertible {
  required init(_ value: Int) {
    trackedCount += 1
    nextTrackedSerialNumber += 1
    serialNumber = nextTrackedSerialNumber
    self.value = value
  }
  
  deinit {
    assert(serialNumber > 0, "double destruction!")
    trackedCount -= 1
    serialNumber = -serialNumber
  }

  var description: String {
    assert(serialNumber > 0, "dead Tracked!")
    return value.description
  }

  func successor() -> Self {
    return self.dynamicType.init(self.value.successor())
  }

  var value: Int
  var serialNumber: Int
}

func == (x: Tracked, y: Tracked) -> Bool {
  return x.value == y.value
}

//===--- struct MrMcRange -------------------------------------------------===//
// A wrapper around Range<Tracked> that allows us to detect when it is
// being treated as a Collection rather than merely a Sequence, which
// helps us to prove that an optimization is being used.  In
// particular, when constructing a _ContiguousArrayBuffer from a
// Collection, the necessary storage should be pre-allocated.
struct MrMcRange : Collection {
  typealias Base = Range<Int>

  init(_ base: Base) {
    self.base = base
  }

  var startIndex: Int {
    print("using collection API")
    return base.startIndex
  }
  
  var endIndex: Int {
    return base.endIndex
  }

  subscript(i: Int) -> Tracked {
    return Tracked(i)
  }
  
  var base: Base
}

let ArrayCoreTests = TestSuite("ArrayCore")

ArrayCoreTests.test("Sequences can be converted") {
  let n = ((Tracked(10)..<Tracked(27)).makeIterator())._copyToNativeArrayBuffer()
  let out = n.map { "\($0)" }.joined(separator: " ")
  expectPrinted("10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26", out)
}

ArrayCoreTests.test("Collections get measured") {
  let n = MrMcRange(3..<23)._copyToNativeArrayBuffer()
  let out = n.map { "\($0)" }.joined(separator: " ")
  expectPrinted("3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22", out)
}

ArrayCoreTests.test("Check trackedCount") {
  expectEqual(0, trackedCount)
}

runAllTests()
