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
    name: "IndexPath.Max.Beginning",
    runFunction: { n in 
      run_IndexPathMaxBeginning(n * 25, decreasingIndexPath)
    },
    tags: tags,
    setUpFunction: { blackHole(decreasingIndexPath) }),
  BenchmarkInfo(
    name: "IndexPath.Max.Middle",
    runFunction: { n in
      run_IndexPathMaxMiddle(n * 25, increasingMaxMiddleIndexPath)
    },
    tags: tags,
    setUpFunction: { blackHole(increasingMaxMiddleIndexPath) }),
  BenchmarkInfo(
    name: "IndexPath.Max.End",
    runFunction: { n in 
      run_IndexPathMaxEnd(n * 25, increasingIndexPath) 
    },
    tags: tags,
    setUpFunction: { blackHole(increasingIndexPath) }),

  BenchmarkInfo(
    name: "IndexPath.Min.Beginning",
    runFunction: { n in 
      run_IndexPathMinBeginning(n * 25, increasingIndexPath)
    },
    tags: tags,
    setUpFunction: { blackHole(increasingIndexPath) }),
  BenchmarkInfo(
    name: "IndexPath.Min.Middle",
    runFunction: { n in
      run_IndexPathMinMiddle(n * 25, increasingMinMiddleIndexPath)
    },
    tags: tags,
    setUpFunction: { blackHole(increasingMinMiddleIndexPath) }),
  BenchmarkInfo(
    name: "IndexPath.Min.End",
    runFunction: { n in 
      run_IndexPathMinEnd(n * 25, decreasingIndexPath) 
    },
    tags: tags,
    setUpFunction: { blackHole(decreasingIndexPath) }),
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

// Max, Min

@inline(__always)
func maxMin(
  n: Int,
  indexPath: IndexPath,
  maxMinFunc: (inout IndexPath) -> Int?
) {
  for _ in 0..<n {
    var ip = indexPath
    let found = maxMinFunc(&ip) != nil
    blackHole(found)
  }
}

// Max

@inline(never)
public func run_IndexPathMaxBeginning(_ n: Int, _ indexPath: IndexPath) {
  maxMin(
    n: n,
    indexPath: indexPath,
    maxMinFunc: { ip in
      ip.max()
    })
}

@inline(never)
public func run_IndexPathMaxMiddle(_ n: Int, _ indexPath: IndexPath) {
  maxMin(
    n: n,
    indexPath: indexPath,
    maxMinFunc: { ip in
      ip.max()
    })
}

@inline(never)
public func run_IndexPathMaxEnd(_ n: Int, _ indexPath: IndexPath) {
  maxMin(
    n: n,
    indexPath: indexPath,
    maxMinFunc: { ip in
      ip.max()
    })
}

// Min

@inline(never)
public func run_IndexPathMinBeginning(_ n: Int, _ indexPath: IndexPath) {
  maxMin(
    n: n,
    indexPath: indexPath,
    maxMinFunc: { ip in
      ip.min()
    })
}

@inline(never)
public func run_IndexPathMinMiddle(_ n: Int, _ indexPath: IndexPath) {
  maxMin(
    n: n,
    indexPath: indexPath,
    maxMinFunc: { ip in
      ip.min()
    })
}

@inline(never)
public func run_IndexPathMinEnd(_ n: Int, _ indexPath: IndexPath) {
  maxMin(
    n: n,
    indexPath: indexPath,
    maxMinFunc: { ip in
      ip.min()
    })
}
