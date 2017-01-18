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

@inline(never)
public func run_DictionarySwap(_ N: Int) {
    let size = 100
    var dict = [Int: Int](minimumCapacity: size)

    // Fill dictionary
    for i in 1...size {
        dict[i] = i
    }
    CheckResults(dict.count == size,
                 "Incorrect dict count: \(dict.count) != \(size).")

    var swapped = false
    for _ in 1...10000*N {
        swap(&dict[25]!, &dict[75]!)
        swapped = !swapped
        if !swappedCorrectly(swapped, dict[25]!, dict[75]!) {
            break
        }
    }

    CheckResults(swappedCorrectly(swapped, dict[25]!, dict[75]!),
                 "Dictionary value swap failed")
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

  var hashValue: Int {
    return value.hashValue
  }

  static func ==<T: Equatable>(lhs: Box<T>, rhs: Box<T>) -> Bool {
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
    CheckResults(dict.count == size,
                 "Incorrect dict count: \(dict.count) != \(size).")

    var swapped = false
    for _ in 1...10000*N {
        swap(&dict[Box(25)]!, &dict[Box(75)]!)
        swapped = !swapped
        if !swappedCorrectly(swapped, dict[Box(25)]!.value, dict[Box(75)]!.value) {
            break
        }
    }

    CheckResults(swappedCorrectly(swapped, dict[Box(25)]!.value, dict[Box(75)]!.value),
                 "Dictionary value swap failed")
}
