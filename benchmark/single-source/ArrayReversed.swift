//===--- ArrayReversed.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This benchmark compares the performance of iteration through an array in
// both forward and reverse.

@inline(never)
public func run_ArrayReversed(_ N: Int) {

  let array = [Int](repeating: 1, count: 100_000)
  let reversed_array = array.reversed()
  var x = array[0]

  // Measure the time to iterate with a for-in 
  // loop over a ReversedRandomAccessCollection.
  for _ in 0..<N {
    for item in reversed_array {
      x = item
    }
  }
}
