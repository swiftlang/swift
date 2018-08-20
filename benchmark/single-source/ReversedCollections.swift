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

import TestsUtils

public let ReversedCollections = [
  BenchmarkInfo(name: "ReversedArray2", runFunction: run_ReversedArray, tags: [.validation, .api, .Array],
      setUpFunction: { blackHole(arrayInput) },
      tearDownFunction: { arrayInput = nil }),
  BenchmarkInfo(name: "ReversedBidirectional", runFunction: run_ReversedBidirectional, tags: [.validation, .api]),
  BenchmarkInfo(name: "ReversedDictionary2", runFunction: run_ReversedDictionary, tags: [.validation, .api, .Dictionary],
      setUpFunction: { blackHole(dictionaryInput) },
      tearDownFunction: { dictionaryInput = nil })
]

// These benchmarks compare the performance of iteration through several
// collection types after being reversed.
let length = 100_000

var arrayInput: [Int]! = Array(repeating: 1, count: length).reversed()

@inline(never)
public func run_ReversedArray(_ N: Int) {
  let reversedArray: [Int] = arrayInput

  // Iterate over the underlying type
  // ReversedRandomAccessCollection<Array<Int>>
  for _ in 1...N {
    for item in reversedArray {
      blackHole(item)
    }
  }
}

@inline(never)
public func run_ReversedBidirectional(_ N: Int) {
  // Iterate over the underlying type
  // ReversedCollection<AnyBidirectionalCollection<Int>>
  for _ in 1...N {
    let bidirectional = AnyBidirectionalCollection(0..<length)
    let reversedBidirectional = bidirectional.reversed()
    for item in reversedBidirectional {
      blackHole(item)
    }
  }
}

var dictionaryInput: [(Int, Int)]! = {
  var dictionary = [Int: Int]()
  for k in 0..<length {
    dictionary[k] = k
  }
  return dictionary.reversed()
}()

@inline(never)
public func run_ReversedDictionary(_ N: Int) {
  let reversedDictionary: [(Int, Int)] = dictionaryInput

  // Iterate over the underlying type
  // Array<(Int, Int)>
  for _ in 1...N {
    for (key, value) in reversedDictionary {
      blackHole(key)
      blackHole(value)
    }
  }
}
