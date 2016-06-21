//===--- PopFrontGeneric.swift --------------------------------------------===//
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

import TestsUtils

let reps = 1
let arrayCount = 1024

// This test case exposes rdar://17440222 which caused rdar://17974483 (popFront
// being really slow).

func _arrayReplace<B: _ArrayBufferProtocol, C: Collection
  where C.Iterator.Element == B.Element, B.Index == Int
  >(
  _ target: inout B, _ subRange: Range<Int>, _ newValues: C
) {
  _precondition(
    subRange.lowerBound >= 0,
    "Array replace: subRange start is negative")

  _precondition(
    subRange.upperBound <= target.endIndex,
    "Array replace: subRange extends past the end")

  let oldCount = target.count
  let eraseCount = subRange.count
  let insertCount = numericCast(newValues.count) as Int
  let growth = insertCount - eraseCount

  if target.requestUniqueMutableBackingBuffer(minimumCapacity: oldCount + growth) != nil {
    target.replace(subRange: subRange, with: insertCount, elementsOf: newValues)
  }
  else {
    _preconditionFailure("Should not get here?")
  }
}


@inline(never)
public func run_PopFrontArrayGeneric(_ N: Int) {
  let orig = Array(repeating: 1, count: arrayCount)
  var a = [Int]()
  for _ in 1...20*N {
    for _ in 1...reps {
      var result = 0
      a.append(contentsOf: orig)
      while a.count != 0 {
        result += a[0]
        _arrayReplace(&a._buffer, 0..<1, EmptyCollection())
      }
      CheckResults(result == arrayCount, "IncorrectResults in StringInterpolation: \(result) != \(arrayCount)")
    }
  }
}
