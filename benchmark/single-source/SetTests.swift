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

public let SetTests = [
  BenchmarkInfo(name: "SetExclusiveOr2", runFunction: run_SetExclusiveOr2, tags: [.validation, .api, .Set]),
  BenchmarkInfo(name: "SetExclusiveOr2_OfObjects", runFunction: run_SetExclusiveOr2_OfObjects, tags: [.validation, .api, .Set]),
  BenchmarkInfo(name: "SetIntersect2", runFunction: run_SetIntersect2, tags: [.validation, .api, .Set]),
  BenchmarkInfo(name: "SetIntersect2_OfObjects", runFunction: run_SetIntersect2_OfObjects, tags: [.validation, .api, .Set]),
  BenchmarkInfo(name: "SetIsSubsetOf2", runFunction: run_SetIsSubsetOf2, tags: [.validation, .api, .Set]),
  BenchmarkInfo(name: "SetIsSubsetOf2_OfObjects", runFunction: run_SetIsSubsetOf2_OfObjects, tags: [.validation, .api, .Set]),
  BenchmarkInfo(name: "SetUnion2", runFunction: run_SetUnion2, tags: [.validation, .api, .Set]),
  BenchmarkInfo(name: "SetUnion2_OfObjects", runFunction: run_SetUnion2_OfObjects, tags: [.validation, .api, .Set]),
]

@inline(never)
public func run_SetIsSubsetOf2(_ N: Int) {
  let size = 400

  let set = Set<Int>(size / 2 ..< size)
  let otherSet = Set<Int>(0 ..< size)

  for _ in 0 ..< N * 5000 {
    let isSubset = set.isSubset(of: identity(otherSet))
    CheckResults(isSubset)
  }
}

@inline(never)
public func run_SetExclusiveOr2(_ N: Int) {
  let size = 400
  let overlap = 100

  let set = Set<Int>(0 ..< size)
  let otherSet = Set<Int>(size - overlap ..< 2 * size - overlap)

  for _ in 0 ..< N * 100 {
    let diff = set.symmetricDifference(identity(otherSet))
    CheckResults(diff.count == 2 * (size - overlap))
  }
}

@inline(never)
public func run_SetUnion2(_ N: Int) {
  let size = 400
  let overlap = 100

  let set = Set<Int>(0 ..< size)
  let otherSet = Set<Int>(size - overlap ..< 2 * size - overlap)

  for _ in 0 ..< N * 100 {
    let or = set.union(identity(otherSet))
    CheckResults(or.count == 2 * size)
  }
}

@inline(never)
public func run_SetIntersect2(_ N: Int) {
  let size = 400
  let overlap = 100

  let set = Set<Int>(0 ..< size)
  let otherSet = Set<Int>(size - overlap ..< 2 * size - overlap)

  for _ in 0 ..< N * 100 {
    let and = set.intersection(identity(otherSet))
    CheckResults(and.count == overlap)
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
public func run_SetIsSubsetOf_OfObjects(_ N: Int) {
  let size = 400

  let set = Set<Box<Int>>((size / 2 ..< size).map(Box.init))
  let otherSet = Set<Box<Int>>((0 ..< size).map(Box.init))

  for _ in 0 ..< N * 5000 {
    let isSubset = set.isSubset(of: identity(otherSet))
    CheckResults(isSubset)
  }
}

@inline(never)
public func run_SetExclusiveOr_OfObjects(_ N: Int) {
  let size = 400
  let overlap = 100

  let set = Set<Box<Int>>((0 ..< size).map(Box.init))
  let otherSet = Set<Box<Int>>(
    (size - overlap ..< 2 * size - overlap).map(Box.init))

  for _ in 0 ..< N * 100 {
    let diff = set.symmetricDifference(identity(otherSet))
    CheckResults(diff.count == 2 * (size - overlap))
  }
}

@inline(never)
public func run_SetUnion_OfObjects(_ N: Int) {
  let size = 400
  let overlap = 100

  let set = Set<Box<Int>>((0 ..< size).map(Box.init))
  let otherSet = Set<Box<Int>>(
    (size - overlap ..< 2 * size - overlap).map(Box.init))

  for _ in 0 ..< N * 100 {
    let or = set.union(identity(otherSet))
    CheckResults(or.count == 2 * size)
  }
}

@inline(never)
public func run_SetIntersect2_OfObjects(_ N: Int) {
  let size = 400
  let overlap = 100

  let set = Set<Box<Int>>((0 ..< size).map(Box.init))
  let otherSet = Set<Box<Int>>(
    (size - overlap ..< 2 * size - overlap).map(Box.init))

  for _ in 0 ..< N * 100 {
    let and = set.intersection(otherSet)
    CheckResults(and.count == overlap)
  }
}
