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
    name: "FlattenDistanceFromTo.Array.Array.04.04",
    runFunction: { with(arrayArray04x04, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayArray04x04) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.Array.04x08",
    runFunction: { with(arrayArray04x08, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayArray04x08) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.Array.08.04",
    runFunction: { with(arrayArray08x04, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayArray08x04) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.Array.08.08",
    runFunction: { with(arrayArray08x08, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayArray08x08) }),
  
  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.String.04.04",
    runFunction: { with(arrayString04x04, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayString04x04) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.String.04.08",
    runFunction: { with(arrayString04x08, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayString04x08) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.String.08.04",
    runFunction: { with(arrayString08x04, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayString08x04) }),

  BenchmarkInfo(
    name: "FlattenDistanceFromTo.Array.String.08.08",
    runFunction: { with(arrayString08x08, $0) },
    tags: [.validation, .api],
    setUpFunction: { blackHole(arrayString08x08) }),
]

// MARK: - Array Array

func makeArrayArray(_ outer: Int, _ inner: Int) -> FlattenSequence<[[UInt8]]> {
  Array(repeating: Array(repeating: 123, count: inner), count: outer).joined()
}

let arrayArray04x04 = makeArrayArray(04, 04)
let arrayArray04x08 = makeArrayArray(04, 08)
let arrayArray08x04 = makeArrayArray(08, 04)
let arrayArray08x08 = makeArrayArray(08, 08)

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

let arrayString04x04 = makeArrayString(04, 04)
let arrayString04x08 = makeArrayString(04, 08)
let arrayString08x04 = makeArrayString(08, 04)
let arrayString08x08 = makeArrayString(08, 08)

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
