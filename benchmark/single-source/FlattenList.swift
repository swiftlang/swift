//===--- FlattenList.swift ------------------------------------------------===//
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

public let FlattenListLoop = BenchmarkInfo(
  name: "FlattenListLoop",
  runFunction: run_FlattenListLoop,
  tags: [.api, .validation],
  setUpFunction: { blackHole(inputArray) })

public let FlattenListFlatMap = BenchmarkInfo(
  name: "FlattenListFlatMap",
  runFunction: run_FlattenListFlatMap,
  tags: [.api, .validation],
  setUpFunction: { blackHole(inputArray) })

let inputArray: [(Int, Int, Int, Int)] = (0..<(1<<16)).map { _ in
  (5, 6, 7, 8)
}

func flattenFlatMap(_ input: [(Int, Int, Int, Int)]) -> [Int] {
  return input.flatMap { [$0.0, $0.1, $0.2, $0.3] }
}

func flattenLoop(_ input: [(Int, Int, Int, Int)]) -> [Int] {
  var flattened: [Int] = []
  flattened.reserveCapacity(input.count * 4)

  for (x, y, z, w) in input {
    flattened.append(x)
    flattened.append(y)
    flattened.append(z)
    flattened.append(w)
  }

  return flattened
}

@inline(never)
public func run_FlattenListLoop(_ N: Int) {
  for _ in 0..<5*N {
    blackHole(flattenLoop(inputArray))
  }
}

@inline(never)
public func run_FlattenListFlatMap(_ N: Int) {
  for _ in 1...5*N {
    blackHole(flattenFlatMap(inputArray))
  }
}
