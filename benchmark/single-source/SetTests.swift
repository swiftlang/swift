//===--- SetTests.swift ---------------------------------------------------===//
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

let size = 400
let half = size / 2
let quarter = size / 4

// Construction kit for sets with 25% overlap
let setAB = Set(0 ..< size)                              //   0 ..< 400
let setCD = Set(size ..< 2 * size)                       // 400 ..< 800
let setBC = Set(size - quarter ..< 2 * size - quarter)   // 300 ..< 700
let setB = Set(size - quarter ..< size)                  // 300 ..< 400

let setOAB = Set(setAB.map(Box.init))
let setOCD = Set(setCD.map(Box.init))
let setOBC = Set(setBC.map(Box.init))
let setOB = Set(setB.map(Box.init))

let countA = size - quarter        // 300
let countB = quarter               // 100
let countC = countA                // 300
let countD = countB                // 100

let countAB = size                 // 400
let countAC = countA + countC      // 600
let countABC = countA + countB + countC           // 700
let countABCD = countA + countB + countC + countD // 800

// Construction kit for sets with 50% overlap
let setXY = Set(0 ..< size)           //   0 ..< 400
let setYZ = Set(half ..< size + half) // 200 ..< 600
let setY = Set(half ..< size)         // 200 ..< 400

// Two sets with 100% overlap, but different bucket counts (let's not make it
// too easy...)
let setP = Set(0 ..< size)
let setQ: Set<Int> = {
  var set = Set(0 ..< size)
  set.reserveCapacity(2 * size)
  return set
}()

public let SetTests = [
  // Mnemonic: number after name is percentage of common elements in input sets.
  BenchmarkInfo(
    name: "SetIsSubsetInt0",
    runFunction: { n in run_SetIsSubsetInt(setAB, setCD, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetIsSubsetBox0",
    runFunction: { n in run_SetIsSubsetBox(setOAB, setOCD, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetIsSubsetInt25",
    runFunction: { n in run_SetIsSubsetInt(setB, setAB, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setB, setAB]) }),
  BenchmarkInfo(
    name: "SetIsSubsetBox25",
    runFunction: { n in run_SetIsSubsetBox(setOB, setOAB, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOB, setOAB]) }),
  BenchmarkInfo(
    name: "SetIsSubsetInt50",
    runFunction: { n in run_SetIsSubsetInt(setY, setXY, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setY, setXY]) }),
  BenchmarkInfo(
    name: "SetIsSubsetInt100",
    runFunction: { n in run_SetIsSubsetInt(setP, setQ, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "SetSymmetricDifferenceInt0",
    runFunction: { n in run_SetSymmetricDifferenceInt(setAB, setCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetSymmetricDifferenceBox0",
    runFunction: { n in run_SetSymmetricDifferenceBox(setOAB, setOCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetSymmetricDifferenceInt25",
    runFunction: { n in run_SetSymmetricDifferenceInt(setAB, setBC, countAC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetSymmetricDifferenceBox25",
    runFunction: { n in run_SetSymmetricDifferenceBox(setOAB, setOBC, countAC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),
  BenchmarkInfo(
    name: "SetSymmetricDifferenceInt50",
    runFunction: { n in run_SetSymmetricDifferenceInt(setXY, setYZ, size, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setXY, setYZ]) }),
  BenchmarkInfo(
    name: "SetSymmetricDifferenceInt100",
    runFunction: { n in run_SetSymmetricDifferenceInt(setP, setQ, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "SetIntersectionInt0",
    runFunction: { n in run_SetIntersectionInt(setAB, setCD, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetIntersectionBox0",
    runFunction: { n in run_SetIntersectionBox(setOAB, setOCD, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetIntersectionInt25",
    runFunction: { n in run_SetIntersectionInt(setAB, setBC, countB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetIntersectionBox25",
    runFunction: { n in run_SetIntersectionBox(setOAB, setOBC, countB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),
  BenchmarkInfo(
    name: "SetIntersectionInt50",
    runFunction: { n in run_SetIntersectionInt(setXY, setYZ, half, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setXY, setYZ]) }),
  BenchmarkInfo(
    name: "SetIntersectionInt100",
    runFunction: { n in run_SetIntersectionInt(setP, setQ, size, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "SetUnionInt0",
    runFunction: { n in run_SetUnionInt(setAB, setCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetUnionBox0",
    runFunction: { n in run_SetUnionBox(setOAB, setOCD, countABCD, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetUnionInt25",
    runFunction: { n in run_SetUnionInt(setAB, setBC, countABC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetUnionBox25",
    runFunction: { n in run_SetUnionBox(setOAB, setOBC, countABC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),
  BenchmarkInfo(
    name: "SetUnionInt50",
    runFunction: { n in run_SetUnionInt(setXY, setYZ, size + half, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setXY, setYZ]) }),
  BenchmarkInfo(
    name: "SetUnionInt100",
    runFunction: { n in run_SetUnionInt(setP, setQ, size, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  BenchmarkInfo(
    name: "SetSubtractingInt0",
    runFunction: { n in run_SetSubtractingInt(setAB, setCD, countAB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetSubtractingBox0",
    runFunction: { n in run_SetSubtractingBox(setOAB, setOCD, countAB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetSubtractingInt25",
    runFunction: { n in run_SetSubtractingInt(setAB, setBC, countA, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetSubtractingBox25",
    runFunction: { n in run_SetSubtractingBox(setOAB, setOBC, countA, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),
  BenchmarkInfo(
    name: "SetSubtractingInt50",
    runFunction: { n in run_SetSubtractingInt(setXY, setYZ, half, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setXY, setYZ]) }),
  BenchmarkInfo(
    name: "SetSubtractingInt100",
    runFunction: { n in run_SetSubtractingInt(setP, setQ, 0, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setP, setQ]) }),

  // Legacy benchmarks, kept for continuity with previous releases.
  BenchmarkInfo(
    name: "SetExclusiveOr", // ~"SetSymmetricDifferenceInt0"
    runFunction: { n in run_SetSymmetricDifferenceInt(setAB, setCD, countABCD, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetExclusiveOr_OfObjects", // ~"SetSymmetricDifferenceBox0"
    runFunction: { n in run_SetSymmetricDifferenceBox(setOAB, setOCD, countABCD, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetIntersect", // ~"SetIntersectionInt0"
    runFunction: { n in run_SetIntersectionInt(setAB, setCD, 0, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetUnion", // ~"SetUnionInt0"
    runFunction: { n in run_SetUnionInt(setAB, setCD, countABCD, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetUnion_OfObjects", // ~"SetUnionBox0"
    runFunction: { n in run_SetUnionBox(setOAB, setOCD, countABCD, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
]

@inline(never)
public func run_SetIsSubsetInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isSubset = a.isSubset(of: identity(b))
    CheckResults(isSubset == r)
  }
}

@inline(never)
public func run_SetSymmetricDifferenceInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let diff = a.symmetricDifference(identity(b))
    CheckResults(diff.count == r)
  }
}

@inline(never)
public func run_SetUnionInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let or = a.union(identity(b))
    CheckResults(or.count == r)
  }
}

@inline(never)
public func run_SetIntersectionInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.intersection(identity(b))
    CheckResults(and.count == r)
  }
}

@inline(never)
public func run_SetSubtractingInt(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.subtracting(identity(b))
    CheckResults(and.count == r)
  }
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
func run_SetIsSubsetBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Bool,
  _ n: Int) {
  for _ in 0 ..< n {
    let isSubset = a.isSubset(of: identity(b))
    CheckResults(isSubset == r)
  }
}

@inline(never)
func run_SetSymmetricDifferenceBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let diff = a.symmetricDifference(identity(b))
    CheckResults(diff.count == r)
  }
}

@inline(never)
func run_SetUnionBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let or = a.union(identity(b))
    CheckResults(or.count == r)
  }
}

@inline(never)
func run_SetIntersectionBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.intersection(b)
    CheckResults(and.count == r)
  }
}

@inline(never)
func run_SetSubtractingBox(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.subtracting(b)
    CheckResults(and.count == r)
  }
}
