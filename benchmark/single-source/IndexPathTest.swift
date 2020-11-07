//===--- IndexPathTest.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import TestsUtils

let size = 200
let increasingIndexPath = indexPath(size)
let decreasingIndexPath = indexPath(size, reversed: true)
let increasingMaxMiddleIndexPath = indexPath(size, middle: size + 1)
let increasingMinMiddleIndexPath = indexPath(size, middle: -1)
let tags: [BenchmarkCategory] = [.validation, .api, .IndexPath]

public let IndexPathTest = [
  BenchmarkInfo(
    name: "IndexPath.Subscript.Mutation",
    runFunction: { n in
      run_IndexPathSubscriptMutation(n * 10, size, increasingIndexPath)
    },
    tags: tags,
    setUpFunction: { blackHole(increasingIndexPath) }),
  BenchmarkInfo(
    name: "IndexPath.Subscript.Range.Mutation",
    runFunction: { n in
      run_IndexPathSubscriptRangeMutation(n, size, increasingIndexPath)
    },
    tags: tags,
    setUpFunction: { blackHole(increasingIndexPath) }),
  BenchmarkInfo(
    name: "IndexPath.Max",
    runFunction: { n in run_IndexPathMax(n * 10) },
    tags: tags),
  BenchmarkInfo(
    name: "IndexPath.Min",
    runFunction: { n in run_IndexPathMin(n * 10) },
    tags: tags),
]

@inline(__always)
func indexPath(_ size: Int, reversed: Bool = false) -> IndexPath {
  let indexes = Array(0..<size)
  return IndexPath(indexes: reversed ? indexes.reversed() : indexes)
}

@inline(__always)
func indexPath(_ size: Int, middle: Int) -> IndexPath {
  var indexes = Array(0..<size)
  indexes.insert(middle, at: (indexes.count - 1) / 2)
  return IndexPath(indexes: indexes)
}

// Subscript Mutations

@inline(__always)
func subscriptMutation(
  n: Int,
  mutations: Int,
  indexPath: IndexPath,
  mutate: (inout IndexPath, Int) -> Void
) {
  for _ in 0..<n {
    for i in 0..<mutations {
      var ip = indexPath
      mutate(&ip, i)
    }
  }
}

@inline(never)
public func run_IndexPathSubscriptMutation(
  _ n: Int, _ count: Int, _ indexPath: IndexPath
) {
  subscriptMutation(
    n: n, mutations: count, indexPath: indexPath,
    mutate: { ip, i in
      ip[i % 4] += 1
    })
}

@inline(never)
public func run_IndexPathSubscriptRangeMutation(
  _ n: Int, _ count: Int, _ indexPath: IndexPath
) {
  subscriptMutation(
    n: n, mutations: count, indexPath: indexPath,
    mutate: { ip, i in
      ip[0..<i] += [i]
    })
}

// Max

@inline(never)
public func run_IndexPathMax(_ n: Int) {
  for _ in 0..<n {
    var val: Int?
    // Beginning max
    val = decreasingIndexPath.max()
    // Middle max
    val = increasingMaxMiddleIndexPath.max()
    // End max
    val = increasingIndexPath.max()
    blackHole(val)
  }
}

// Min

@inline(never)
public func run_IndexPathMin(_ n: Int) {
  for _ in 0..<n {
    var val: Int?
    // Beginning min
    val = increasingIndexPath.min()
    // Middle min
    val = increasingMinMiddleIndexPath.min()
    // End min
    val = decreasingIndexPath.min()
    blackHole(val)
  }
}
