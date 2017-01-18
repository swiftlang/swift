//===--- RecursiveOwnedParameter.swift ------------------------------------===//
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

import TestsUtils

// This test recursively visits each element of an array in a class and compares
// it with every value in a different array stored in a different class. The
// idea is to make sure that we can get rid of the overhead from guaranteed
// parameters.

// We use final since we are not interesting in devirtualization for the
// purposes of this test.
final
class ArrayWrapper {
  var data: [Int]

  init(_ count: Int, _ initialValue: Int) {
    data = [Int](repeating: initialValue, count: count)
  }

  func compare(_ b: ArrayWrapper, _ iteration: Int, _ stopIteration: Int) -> Bool {
    // We will never return true here by design. We want to test the full effect
    // every time of retaining, releasing.
    if iteration == stopIteration || data[iteration] == b.data[iteration] {
      return true
    }

    return compare(b, iteration &+ 1, stopIteration)
  }
}

@inline(never)
public func run_RecursiveOwnedParameter(_ N: Int) {
  let numElts = 1_000

  let a = ArrayWrapper(numElts, 0)
  let b = ArrayWrapper(numElts, 1)

  var result = 0
  for _ in 0..<100*N {
    if a.compare(b, 0, numElts) {
      result += 1
    }
  }
  let refResult = 100*N
  CheckResults(result == refResult,
    "IncorrectResults in RecursiveOwnedParameter: \(result) != \(refResult)")
}
