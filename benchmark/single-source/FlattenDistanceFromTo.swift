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
    runFunction: { withRandomAccess(16, 16, $0) },
    tags: [.validation, .api]),
  
  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RandomAccess.16x32",
    runFunction: { withRandomAccess(16, 32, $0) },
    tags: [.validation, .api]),
  
  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RandomAccess.32x16",
    runFunction: { withRandomAccess(32, 16, $0) },
    tags: [.validation, .api]),
  
  BenchmarkInfo(
    name: "FlattenDistanceFromTo.RandomAccess.32x32",
    runFunction: { withRandomAccess(32, 32, $0) },
    tags: [.validation, .api]),
]

@inline(never)
public func withRandomAccess(
  _ outer: Int,
  _ inner: Int,
  _ iterations: Int
) {
  var value = 0 as Int
  let minor = repeatElement(00000, count: inner)
  let major = repeatElement(minor, count: outer)
  let flattened: FlattenSequence = major.joined()
  
  for _ in 0 ..< iterations {
    for a in flattened.indices {
      for b in flattened.indices {
        value &+= flattened.distance(from: a, to: b)
        value &+= flattened.distance(from: b, to: a)
      }
    }
  }
  
  blackHole(value == 0)
}
