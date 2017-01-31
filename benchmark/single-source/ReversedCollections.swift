//===--- ReversedCollections.swift ----------------------------------------===//
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

// These benchmarks compare the performance of iteration through several
// collection types after being reversed.
var x = 0
let length = 100_000

@inline(never)
public func run_ReversedArray(_ N: Int) {
  let array = Array(repeating: 1, count: length)
  let reversedArray = array.reversed()

  // Iterate over the underlying type
  // ReversedRandomAccessCollection<Array<Int>>
  for _ in 1...N {
    for item in reversedArray {
      x = item
    }
  }
}

@inline(never)
public func run_ReversedBidirectional(_ N: Int) {
  let bidirectional = AnyBidirectionalCollection(0..<length)
  let reversedBidirectional = bidirectional.reversed()

  // Iterate over the underlying type
  // ReversedCollection<AnyBidirectionalCollection<Int>>
  for _ in 1...N {
    for item in reversedBidirectional {
      x = item
    }
  }
}

@inline(never)
public func run_ReversedDictionary(_ N: Int) {
  var dictionary = [Int: Int]()
  for k in 0..<length {
    dictionary[k] = x
  }
  let reversedDictionary = dictionary.reversed()

  // Iterate over the underlying type
  // Array<(Int, Int)>
  for _ in 1...N {
    for (key, value) in reversedDictionary {
      x = key
      x = value
    }
  }
}
