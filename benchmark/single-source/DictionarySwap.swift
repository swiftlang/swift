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

let size = 100
let numberMap = Dictionary(uniqueKeysWithValues: zip(1...size, 1...size))
let boxedNums = (1...size).lazy.map { Box($0) }
let boxedNumMap = Dictionary(uniqueKeysWithValues: zip(boxedNums, boxedNums))

let t: [BenchmarkCategory] = [.validation, .api, .Dictionary, .cpubench]

public let DictionarySwap = [
  BenchmarkInfo(name: "DictionarySwap",
    runFunction: swap, tags: t, legacyFactor: 4),
  BenchmarkInfo(name: "DictionarySwapOfObjects",
    runFunction: swapObjects, tags: t, legacyFactor: 40),
  BenchmarkInfo(name: "DictionarySwapAt",
    runFunction: swapAt, tags: t, legacyFactor: 4),
  BenchmarkInfo(name: "DictionarySwapAtOfObjects",
    runFunction: swapAtObjects, tags: t, legacyFactor: 11),
]

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

func swap(N: Int) {
  var dict = numberMap
  var swapped = false
  for _ in 1...2500*N {
      (dict[25], dict[75]) = (dict[75]!, dict[25]!)
      swapped = !swapped
      CheckResults(swappedCorrectly(swapped, dict[25]!, dict[75]!))
    }
}

func swapObjects(N: Int) {
  var dict = boxedNumMap
  var swapped = false
  for _ in 1...250*N {
    let b1 = Box(25)
    let b2 = Box(75)
    (dict[b1], dict[b2]) = (dict[b2]!, dict[b1]!)
    swapped = !swapped
    CheckResults(swappedCorrectly(swapped,
      dict[Box(25)]!.value, dict[Box(75)]!.value))
  }
}

func swapAt(N: Int) {
  var dict = numberMap
  var swapped = false
  for _ in 1...2500*N {
    let i25 = dict.index(forKey: 25)!
    let i75 = dict.index(forKey: 75)!
    dict.values.swapAt(i25, i75)
    swapped = !swapped
    CheckResults(swappedCorrectly(swapped, dict[25]!, dict[75]!))
  }
}

func swapAtObjects(N: Int) {
  var dict = boxedNumMap
  var swapped = false
  for _ in 1...1000*N {
    let i25 = dict.index(forKey: Box(25))!
    let i75 = dict.index(forKey: Box(75))!
    dict.values.swapAt(i25, i75)
    swapped = !swapped
    CheckResults(swappedCorrectly(swapped,
      dict[Box(25)]!.value, dict[Box(75)]!.value))
}}
