//===--- DictionarySwap.swift ---------------------------------------------===//
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

// Dictionary element swapping benchmark
// rdar://problem/19804127
import TestsUtils

public let DictionarySwap = [
  BenchmarkInfo(name: "DictionarySwap", runFunction: run_DictionarySwap, tags: [.validation, .api, .Dictionary]),
  BenchmarkInfo(name: "DictionarySwapOfObjects", runFunction: run_DictionarySwapOfObjects, tags: [.validation, .api, .Dictionary]),
  BenchmarkInfo(name: "DictionarySwapAt", runFunction: run_DictionarySwapAt, tags: [.validation, .api, .Dictionary]),
  BenchmarkInfo(name: "DictionarySwapAtOfObjects", runFunction: run_DictionarySwapAtOfObjects, tags: [.validation, .api, .Dictionary]),
]

@inline(never)
public func run_DictionarySwap(_ N: Int) {
    let size = 100
    var dict = [Int: Int](minimumCapacity: size)

    // Fill dictionary
    for i in 1...size {
        dict[i] = i
    }
    CheckResults(dict.count == size)

    var swapped = false
    for _ in 1...10000*N {
        (dict[25], dict[75]) = (dict[75]!, dict[25]!)
        swapped = !swapped
        if !swappedCorrectly(swapped, dict[25]!, dict[75]!) {
            break
        }
    }

    CheckResults(swappedCorrectly(swapped, dict[25]!, dict[75]!))
}

@inline(never)
public func run_DictionarySwapAt(_ N: Int) {
  let size = 100
  var dict = [Int: Int](minimumCapacity: size)

  // Fill dictionary
  for i in 1...size {
    dict[i] = i
  }
  CheckResults(dict.count == size)

  var swapped = false
  for _ in 1...10000*N {
    let i25 = dict.index(forKey: 25)!
    let i75 = dict.index(forKey: 75)!

    dict.values.swapAt(i25, i75)
    swapped = !swapped
    if !swappedCorrectly(swapped, dict[25]!, dict[75]!) {
      break
    }
  }

  CheckResults(swappedCorrectly(swapped, dict[25]!, dict[75]!))
}

// Return true if correctly swapped, false otherwise
func swappedCorrectly(_ swapped: Bool, _ p25: Int, _ p75: Int) -> Bool {
    return swapped && (p25 == 75 && p75 == 25) ||
          !swapped && (p25 == 25 && p75 == 75)
}

class Box<T : Hashable> : Hashable {
  var value: T

  init(_ v: T) {
    value = v
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

  static func ==(lhs: Box, rhs: Box) -> Bool {
    return lhs.value == rhs.value
  }
}

@inline(never)
public func run_DictionarySwapOfObjects(_ N: Int) {
    let size = 100
    var dict = Dictionary<Box<Int>, Box<Int>>(minimumCapacity: size)

    // Fill dictionary
    for i in 1...size {
        dict[Box(i)] = Box(i)
    }
    CheckResults(dict.count == size)

    var swapped = false
    for _ in 1...10000*N {
        let b1 = Box(25)
        let b2 = Box(75)
        (dict[b1], dict[b2]) = (dict[b2]!, dict[b1]!)
        swapped = !swapped
        if !swappedCorrectly(swapped, dict[Box(25)]!.value, dict[Box(75)]!.value) {
            break
        }
    }

    CheckResults(swappedCorrectly(swapped, dict[Box(25)]!.value, dict[Box(75)]!.value))
}

@inline(never)
public func run_DictionarySwapAtOfObjects(_ N: Int) {
  let size = 100
  var dict = [Box<Int>: Box<Int>](minimumCapacity: size)

  // Fill dictionary
  for i in 1...size {
    dict[Box(i)] = Box(i)
  }
  CheckResults(dict.count == size)

  var swapped = false
  for _ in 1...10000*N {
    let b25 = Box(25)
    let b75 = Box(75)
    let i25 = dict.index(forKey: b25)!
    let i75 = dict.index(forKey: b75)!

    dict.values.swapAt(i25, i75)
    swapped = !swapped
    if !swappedCorrectly(swapped, dict[Box(25)]!.value, dict[Box(75)]!.value) {
      break
    }
  }

  CheckResults(swappedCorrectly(swapped, dict[Box(25)]!.value, dict[Box(75)]!.value))
}
