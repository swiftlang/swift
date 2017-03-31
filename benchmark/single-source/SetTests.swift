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

@inline(never)
public func run_SetIsSubsetOf(_ N: Int) {
  let size = 200

  SRand()

  var set = Set<Int>(minimumCapacity: size)
  var otherSet = Set<Int>(minimumCapacity: size)

  for _ in 0 ..< size {
    set.insert(Int(truncatingBitPattern: Random()))
    otherSet.insert(Int(truncatingBitPattern: Random()))
  }

  var isSubset = false
  for _ in 0 ..< N * 5000 {
    isSubset = set.isSubset(of: otherSet)
    if isSubset {
      break
    }
  }

  CheckResults(!isSubset, "Incorrect results in SetIsSubsetOf")
}

@inline(never)
func sink(_ s: inout Set<Int>) {
}

@inline(never)
public func run_SetExclusiveOr(_ N: Int) {
  let size = 400

  SRand()

  var set = Set<Int>(minimumCapacity: size)
  var otherSet = Set<Int>(minimumCapacity: size)

  for _ in 0 ..< size {
    set.insert(Int(truncatingBitPattern: Random()))
    otherSet.insert(Int(truncatingBitPattern: Random()))
  }

  var xor = Set<Int>()
  for _ in 0 ..< N * 100 {
    xor = set.symmetricDifference(otherSet)
  }
  sink(&xor)
}

@inline(never)
public func run_SetUnion(_ N: Int) {
  let size = 400

  SRand()

  var set = Set<Int>(minimumCapacity: size)
  var otherSet = Set<Int>(minimumCapacity: size)

  for _ in 0 ..< size {
    set.insert(Int(truncatingBitPattern: Random()))
    otherSet.insert(Int(truncatingBitPattern: Random()))
  }

  var or = Set<Int>()
  for _ in 0 ..< N * 100 {
    or = set.union(otherSet)
  }
  sink(&or)
}

@inline(never)
public func run_SetIntersect(_ N: Int) {
  let size = 400

  SRand()

  var set = Set<Int>(minimumCapacity: size)
  var otherSet = Set<Int>(minimumCapacity: size)

  for _ in 0 ..< size {
    set.insert(Int(truncatingBitPattern: Random()))
    otherSet.insert(Int(truncatingBitPattern: Random()))
  }

  var and = Set<Int>()
  for _ in 0 ..< N * 100 {
    and = set.intersection(otherSet)
  }
  sink(&and)
}

class Box<T : Hashable> : Hashable {
  var value: T

  init(_ v: T) {
    value = v
  }

  var hashValue: Int {
    return value.hashValue
  }

  static func ==<T>(lhs: Box<T>, rhs: Box<T>) -> Bool {
    return lhs.value == rhs.value
  }
}

@inline(never)
public func run_SetIsSubsetOf_OfObjects(_ N: Int) {
  let size = 200

  SRand()

  var set = Set<Box<Int>>(minimumCapacity: size)
  var otherSet = Set<Box<Int>>(minimumCapacity: size)

  for _ in 0 ..< size {
    set.insert(Box(Int(truncatingBitPattern: Random())))
    otherSet.insert(Box(Int(truncatingBitPattern: Random())))
  }

  var isSubset = false
  for _ in 0 ..< N * 5000 {
    isSubset = set.isSubset(of: otherSet)
    if isSubset {
      break
    }
  }

  CheckResults(!isSubset, "Incorrect results in SetIsSubsetOf")
}

@inline(never)
func sink(_ s: inout Set<Box<Int>>) {
}

@inline(never)
public func run_SetExclusiveOr_OfObjects(_ N: Int) {
  let size = 400

  SRand()

  var set = Set<Box<Int>>(minimumCapacity: size)
  var otherSet = Set<Box<Int>>(minimumCapacity: size)

  for _ in 0 ..< size {
    set.insert(Box(Int(truncatingBitPattern: Random())))
    otherSet.insert(Box(Int(truncatingBitPattern: Random())))
  }

  var xor = Set<Box<Int>>()
  for _ in 0 ..< N * 100 {
    xor = set.symmetricDifference(otherSet)
  }
  sink(&xor)
}

@inline(never)
public func run_SetUnion_OfObjects(_ N: Int) {
  let size = 400

  SRand()

  var set = Set<Box<Int>>(minimumCapacity: size)
  var otherSet = Set<Box<Int>>(minimumCapacity: size)

  for _ in 0 ..< size {
    set.insert(Box(Int(truncatingBitPattern: Random())))
    otherSet.insert(Box(Int(truncatingBitPattern: Random())))
  }

  var or = Set<Box<Int>>()
  for _ in 0 ..< N * 100 {
    or = set.union(otherSet)
  }
  sink(&or)
}

@inline(never)
public func run_SetIntersect_OfObjects(_ N: Int) {
  let size = 400

  SRand()

  var set = Set<Box<Int>>(minimumCapacity: size)
  var otherSet = Set<Box<Int>>(minimumCapacity: size)

  for _ in 0 ..< size {
    set.insert(Box(Int(truncatingBitPattern: Random())))
    otherSet.insert(Box(Int(truncatingBitPattern: Random())))
  }

  var and = Set<Box<Int>>()
  for _ in 0 ..< N * 100 {
    and = set.intersection(otherSet)
  }
  sink(&and)
}
