//===--- DictTest4.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

// This benchmark mostly measures lookups in dictionaries with complex keys,
// using the legacy hashValue API.

public let Dictionary4Legacy = [
  BenchmarkInfo(
    name: "Dictionary4Legacy",
    runFunction: run_Dictionary4Legacy,
    tags: [.validation, .api, .Dictionary]),
  BenchmarkInfo(
    name: "Dictionary4OfObjectsLegacy",
    runFunction: run_Dictionary4OfObjectsLegacy,
    tags: [.validation, .api, .Dictionary]),
]

extension Int {
  mutating func combine(_ value: Int) {
    self = 16777619 &* self ^ value
  }
}

struct LargeKey: Hashable {
  let i: Int
  let j: Int
  let k: Double
  let l: UInt32
  let m: Bool
  let n: Bool
  let o: Bool
  let p: Bool
  let q: Bool

  var hashValue: Int {
    var hash = i.hashValue
    hash.combine(j.hashValue)
    hash.combine(k.hashValue)
    hash.combine(l.hashValue)
    hash.combine(m.hashValue)
    hash.combine(n.hashValue)
    hash.combine(o.hashValue)
    hash.combine(p.hashValue)
    hash.combine(q.hashValue)
    return hash
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(self.hashValue)
  }

  init(_ value: Int) {
    self.i = value
    self.j = 2 * value
    self.k = Double(value) / 7
    self.l = UInt32(truncatingIfNeeded: value)
    self.m = value & 1 == 0
    self.n = value & 2 == 0
    self.o = value & 4 == 0
    self.p = value & 8 == 0
    self.q = value & 16 == 0
  }
}

@inline(never)
public func run_Dictionary4Legacy(_ N: Int) {
  let size1 = 100
  let reps = 20
  let ref_result = "1 99 \(reps) \(reps * 99)"
  var hash1 = [LargeKey: Int]()
  var hash2 = [LargeKey: Int]()
  var res = ""

  for _ in 1...N {
    // Test insertions
    hash1 = [:]
    for i in 0..<size1 {
      hash1[LargeKey(i)] = i
    }

    hash2 = hash1

    // Test lookups & value replacement
    for _ in 1..<reps {
      for (k, v) in hash1 {
        hash2[k] = hash2[k]! + v
      }
    }

    res = "\(hash1[LargeKey(1)]!) \(hash1[LargeKey(99)]!)" +
      " \(hash2[LargeKey(1)]!) \(hash2[LargeKey(99)]!)"
    if res != ref_result {
      break
    }
  }
  CheckResults(res == ref_result)
}

class Box<T : Hashable> : Hashable {
  var value: T

  init(_ v: T) {
    value = v
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

  var hashValue: Int {
    return value.hashValue
  }

  static func ==(lhs: Box, rhs: Box) -> Bool {
    return lhs.value == rhs.value
  }
}

@inline(never)
public func run_Dictionary4OfObjectsLegacy(_ N: Int) {
  let size1 = 100
  let reps = 20
  let ref_result = "1 99 \(reps) \(reps * 99)"
  var hash1 = [Box<LargeKey>: Int]()
  var hash2 = [Box<LargeKey>: Int]()
  var res = ""

  for _ in 1...N {
    // Test insertions
    hash1 = [:]
    for i in 0..<size1 {
      hash1[Box(LargeKey(i))] = i
    }

    hash2 = hash1

    // Test lookups & value replacement
    for _ in 1..<reps {
      for (k, v) in hash1 {
        hash2[k] = hash2[k]! + v
      }
    }

    res = "\(hash1[Box(LargeKey(1))]!) \(hash1[Box(LargeKey(99))]!)" +
      " \(hash2[Box(LargeKey(1))]!) \(hash2[Box(LargeKey(99))]!)"
    if res != ref_result {
      break
    }
  }
  CheckResults(res == ref_result)
}
