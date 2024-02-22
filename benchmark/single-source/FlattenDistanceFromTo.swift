//===--- FlattenDistanceFromTo.swift --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let FlattenDistanceFromTo = [
  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RandomAccess.16x16",
    runFunction: { with(randomAccess16x16, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(randomAccess16x16) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RandomAccess.16x32",
    runFunction: { with(randomAccess16x32, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(randomAccess16x32) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RandomAccess.32x16",
    runFunction: { with(randomAccess32x16, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(randomAccess32x16) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RandomAccess.32x32",
    runFunction: { with(randomAccess32x32, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(randomAccess32x32) }),
  
  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RepeatString.08x08",
    runFunction: { with(repeatString08x08, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(repeatString08x08) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RepeatString.08x16",
    runFunction: { with(repeatString08x16, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(repeatString08x16) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RepeatString.16x08",
    runFunction: { with(repeatString16x08, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(repeatString16x08) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RepeatString.16x16",
    runFunction: { with(repeatString16x16, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(repeatString16x16) }),
]

// MARK: - Random Access

func makeRandomAccess(
  _ outer: Int,
  _ inner: Int
) -> FlattenSequence<Repeated<Repeated<Int>>> {
  repeatElement(repeatElement(0, count: inner), count: outer).joined()
}

let randomAccess16x16 = makeRandomAccess(16, 16)
let randomAccess16x32 = makeRandomAccess(16, 32)
let randomAccess32x16 = makeRandomAccess(32, 16)
let randomAccess32x32 = makeRandomAccess(32, 32)

@inline(never)
public func with(
  _ collection: FlattenSequence<Repeated<Repeated<Int>>>,
  _ iterations: Int
) {
  var value = 0 as Int
  
  for _ in 0 ..< iterations {
    for a in collection.indices {
      for b in collection.indices {
        value &+= collection.distance(from: a, to: b)
        value &+= collection.distance(from: b, to: a)
      }
    }
  }

  blackHole(value == 0)
}

// MARK: - Repeat String

func makeRepeatString(
  _ outer: Int,
  _ inner: Int
) -> FlattenSequence<Repeated<String>> {
  repeatElement(String(repeating: "0", count: inner), count: outer).joined()
}

let repeatString08x08 = makeRepeatString(08, 08)
let repeatString08x16 = makeRepeatString(08, 16)
let repeatString16x08 = makeRepeatString(16, 08)
let repeatString16x16 = makeRepeatString(16, 16)

@inline(never)
public func with(
  _ collection: FlattenSequence<Repeated<String>>,
  _ iterations: Int
) {
  var value = 0 as Int
  
  for _ in 0 ..< iterations {
    for a in collection.indices {
      for b in collection.indices {
        value &+= collection.distance(from: a, to: b)
        value &+= collection.distance(from: b, to: a)
      }
    }
  }

  blackHole(value == 0)
}
