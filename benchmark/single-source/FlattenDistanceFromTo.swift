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

public let benchmarks = [
  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.Array.16.16",
    runFunction: { with(arrayArray16x16, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayArray16x16) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.Array.16.32",
    runFunction: { with(arrayArray16x32, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayArray16x32) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.Array.32.16",
    runFunction: { with(arrayArray32x16, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayArray32x16) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.Array.32.32",
    runFunction: { with(arrayArray32x32, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayArray32x32) }),
  
  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.String.08.08",
    runFunction: { with(arrayString08x08, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayString08x08) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.String.08.16",
    runFunction: { with(arrayString08x16, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayString08x16) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.String.16.08",
    runFunction: { with(arrayString16x08, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayString16x08) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.String.16.16",
    runFunction: { with(arrayString16x16, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayString16x16) }),
]

// MARK: - Array Array

func makeArrayArray(_ outer: Int, _ inner: Int) -> FlattenSequence<[[UInt8]]> {
  Array(repeating: Array(repeating: 123, count: inner), count: outer).joined()
}

let arrayArray16x16 = makeArrayArray(16, 16)
let arrayArray16x32 = makeArrayArray(16, 32)
let arrayArray32x16 = makeArrayArray(32, 16)
let arrayArray32x32 = makeArrayArray(32, 32)

@inline(never)
public func with(_ collection: FlattenSequence<[[UInt8]]>, _ iterations: Int) {
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

// MARK: - Array String

func makeArrayString(_ outer: Int, _ inner: Int) -> FlattenSequence<[String]> {
  Array(repeating: String(repeating: "0", count: inner), count: outer).joined()
}

let arrayString08x08 = makeArrayString(08, 08)
let arrayString08x16 = makeArrayString(08, 16)
let arrayString16x08 = makeArrayString(16, 08)
let arrayString16x16 = makeArrayString(16, 16)

@inline(never)
public func with(_ collection: FlattenSequence<[String]>, _ iterations: Int) {
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
