//===--- ReversedCollections.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(name: "ReversedArray2", runFunction: run_ReversedArray, tags: [.validation, .api, .Array],
      setUpFunction: { blackHole(arrayInput) },
      tearDownFunction: { arrayInput = nil }),
  BenchmarkInfo(name: "ReversedBidirectional", runFunction: run_ReversedBidirectional, tags: [.validation, .api, .cpubench]),
  BenchmarkInfo(name: "ReversedDictionary2", runFunction: run_ReversedDictionary, tags: [.validation, .api, .Dictionary],
      setUpFunction: { blackHole(dictionaryInput) },
      tearDownFunction: { dictionaryInput = nil }),
  BenchmarkInfo(name: "ReversedSequenceToArray", runFunction: run_ReversedSequenceToArray, tags: [.validation, .api, .Array],
      setUpFunction: { blackHole(sequenceInput) },
      tearDownFunction: { sequenceInput = nil }),
  BenchmarkInfo(name: "ReversedShortSequenceToArray", runFunction: run_ReversedShortSequenceToArray, tags: [.validation, .api, .Array],
      setUpFunction: { blackHole(shortSequenceInput) },
      tearDownFunction: { shortSequenceInput = nil })
]

// These benchmarks compare the performance of iteration through several
// collection types after being reversed.
let length = 100_000

var arrayInput: [Int]! = Array(repeating: 1, count: length).reversed()

@inline(never)
public func run_ReversedArray(_ n: Int) {
  let reversedArray: [Int] = arrayInput

  // Iterate over the underlying type
  // ReversedRandomAccessCollection<Array<Int>>
  for _ in 1...n {
    for item in reversedArray {
      blackHole(item)
    }
  }
}

@inline(never)
public func run_ReversedBidirectional(_ n: Int) {
  // Iterate over the underlying type
  // ReversedCollection<AnyBidirectionalCollection<Int>>
  for _ in 1...n {
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
public func run_ReversedDictionary(_ n: Int) {
  let reversedDictionary: [(Int, Int)] = dictionaryInput

  // Iterate over the underlying type
  // Array<(Int, Int)>
  for _ in 1...n {
    for (key, value) in reversedDictionary {
      blackHole(key)
      blackHole(value)
    }
  }
}

// These benchmarks measure building the reversed `Array` of a sequence that is
// not a `BidirectionalCollection`, which is what dispatches to
// `Sequence.reversed() -> [Element]`.

struct OnePassSequence: Sequence {
  var elements: [Int]

  func makeIterator() -> Array<Int>.Iterator {
    return elements.makeIterator()
  }
}

let sequenceLength = 10_000

var sequenceInput: OnePassSequence! = OnePassSequence(
  elements: Array(0..<sequenceLength))

var shortSequenceInput: OnePassSequence! = OnePassSequence(elements: [0])

@inline(never)
public func run_ReversedSequenceToArray(_ n: Int) {
  let sequence = sequenceInput!

  for _ in 1...n {
    blackHole(sequence.reversed())
  }
}

// A single element sequence has nothing to reverse, so this only measures the
// fixed overhead of the algorithm.
@inline(never)
public func run_ReversedShortSequenceToArray(_ n: Int) {
  let sequence = shortSequenceInput!

  for _ in 1...n*1_000 {
    blackHole(sequence.reversed())
  }
}
