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
let overlap = 100

let setAB = Set(0 ..< size)                              //   0 ..< 400
let setCD = Set(size ..< 2 * size)                       // 400 ..< 800
let setBC = Set(size - overlap ..< 2 * size - overlap)   // 300 ..< 700
let setB = Set(size - overlap ..< size)                  // 300 ..< 400

let setOAB = Set(setAB.map(Box.init))
let setOCD = Set(setCD.map(Box.init))
let setOBC = Set(setBC.map(Box.init))
let setOB = Set(setB.map(Box.init))

let countAC = 2 * (size - overlap) // 600
let countABC = 2 * size - overlap  // 700
let countABCD = 2 * size           // 800
let countB = overlap               // 100

public let SetTests = [
  BenchmarkInfo(
    name: "SetExclusiveOr",
    runFunction: { n in run_SetExclusiveOr(setAB, setCD, countABCD, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetExclusiveOr2",
    runFunction: { n in run_SetExclusiveOr(setAB, setBC, countAC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetExclusiveOr_OfObjects",
    runFunction: { n in run_SetExclusiveOr_OfObjects(setOAB, setOCD, countABCD, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetExclusiveOr2_OfObjects",
    runFunction: { n in run_SetExclusiveOr_OfObjects(setOAB, setOBC, countAC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),

  BenchmarkInfo(
    name: "SetIntersect",
    runFunction: { n in run_SetIntersect(setAB, setCD, 0, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetIntersect2",
    runFunction: { n in run_SetIntersect(setAB, setBC, countB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetIntersect_OfObjects",
    runFunction: { n in run_SetIntersect_OfObjects(setOAB, setOCD, 0, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetIntersect2_OfObjects",
    runFunction: { n in run_SetIntersect_OfObjects(setOAB, setOBC, countB, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),

  BenchmarkInfo(
    name: "SetIsSubsetOf",
    runFunction: { n in run_SetIsSubsetOf(setAB, setCD, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetIsSubsetOf2",
    runFunction: { n in run_SetIsSubsetOf(setB, setAB, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setB, setAB]) }),
  BenchmarkInfo(
    name: "SetIsSubsetOf_OfObjects",
    runFunction: { n in run_SetIsSubsetOf_OfObjects(setOAB, setOCD, false, 5000 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetIsSubsetOf2_OfObjects",
    runFunction: { n in run_SetIsSubsetOf_OfObjects(setOB, setOAB, true, 50 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOB, setOAB]) }),

  BenchmarkInfo(
    name: "SetUnion",
    runFunction: { n in run_SetUnion(setAB, setCD, countABCD, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setCD]) }),
  BenchmarkInfo(
    name: "SetUnion2",
    runFunction: { n in run_SetUnion(setAB, setBC, countABC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setAB, setBC]) }),
  BenchmarkInfo(
    name: "SetUnion_OfObjects",
    runFunction: { n in run_SetUnion_OfObjects(setOAB, setOCD, countABCD, 100 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOCD]) }),
  BenchmarkInfo(
    name: "SetUnion2_OfObjects",
    runFunction: { n in run_SetUnion_OfObjects(setOAB, setOBC, countABC, 10 * n) },
    tags: [.validation, .api, .Set],
    setUpFunction: { blackHole([setOAB, setOBC]) }),
]

@inline(never)
public func run_SetIsSubsetOf(
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
public func run_SetExclusiveOr(
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
public func run_SetUnion(
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
public func run_SetIntersect(
  _ a: Set<Int>,
  _ b: Set<Int>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.intersection(identity(b))
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
func run_SetIsSubsetOf_OfObjects(
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
func run_SetExclusiveOr_OfObjects(
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
func run_SetUnion_OfObjects(
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
func run_SetIntersect_OfObjects(
  _ a: Set<Box<Int>>,
  _ b: Set<Box<Int>>,
  _ r: Int,
  _ n: Int) {
  for _ in 0 ..< n {
    let and = a.intersection(b)
    CheckResults(and.count == r)
  }
}
