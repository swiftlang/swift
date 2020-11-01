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

import TestsUtils
import Foundation

let size = 200
let tags: [BenchmarkCategory] = [.validation, .api, .IndexPath]

public let IndexPathTest = [
  BenchmarkInfo(name: "IndexPathSubscriptMutation", 
  runFunction: { n in run_IndexPathSubscriptMutation(n * 1000, size)}, 
  tags: tags),
  BenchmarkInfo(name: "IndexPathSubscriptRangeMutation", 
  runFunction: { n in run_IndexPathSubscriptRangeMutation(n * 1000, size)}, 
  tags: tags),

  BenchmarkInfo(name: "IndexPathMaxBeginning", 
  runFunction: { n in run_IndexPathMaxBeginning(n * 1000)}, 
  tags: tags),
  BenchmarkInfo(name: "IndexPathMaxMiddle", 
  runFunction: { n in run_IndexPathMaxMiddle(n * 1000)}, 
  tags: tags),
  BenchmarkInfo(name: "IndexPathMaxEnd", 
  runFunction: { n in run_IndexPathMaxEnd(n * 1000)}, 
  tags: tags),

  BenchmarkInfo(name: "IndexPathMinBeginning", 
  runFunction: { n in run_IndexPathMinBeginning(n * 1000)}, 
  tags: tags),
  BenchmarkInfo(name: "IndexPathMinMiddle", 
  runFunction: { n in run_IndexPathMinMiddle(n * 1000)}, 
  tags: tags),
  BenchmarkInfo(name: "IndexPathMinEnd", 
  runFunction: { n in run_IndexPathMinEnd(n * 1000)}, 
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
func subscriptMutation(n: Int,
  mutations: Int,
  mutate: (inout IndexPath, Int) -> ()) {
    for _ in 0..<n {
      var ip = indexPath(size)

      for i in 0..<mutations {
        mutate(&ip, i)
      }

      blackHole(ip)
    }
}

@inline(never)
public func run_IndexPathSubscriptMutation(_ n: Int, _ count: Int) {
  subscriptMutation(n: n,  mutations: count, mutate: { ip, i in
		ip[i % 4] += 1
	})
}

@inline(never)
public func run_IndexPathSubscriptRangeMutation(_ n: Int, _ count: Int) {
  subscriptMutation(n: count, mutations: count, mutate: { ip, i in
		ip[0..<i] += [i]
	})
}

// Max, Min

@inline(__always)
func maxMin(n: Int, 
  creator: () -> IndexPath, 
  maxMinFunc: (inout IndexPath) -> Int?) {
    for _ in 0..<n {
      var ip = creator()
      let found = maxMinFunc(&ip) != nil
      blackHole(found)
    }
}

// Max

@inline(never)
public func run_IndexPathMaxBeginning(_ n: Int) {
  maxMin(n: n, creator: {
    indexPath(size, reversed: true)
  }, maxMinFunc: { ip in
    ip.max()
  })
}

@inline(never)
public func run_IndexPathMaxMiddle(_ n: Int) {
  maxMin(n: n, creator: {
    indexPath(size, middle: size + 1)
  }, maxMinFunc: { ip in
    ip.max()
  })
}

@inline(never)
public func run_IndexPathMaxEnd(_ n: Int) {
  maxMin(n: n, creator: {
    indexPath(size)
  }, maxMinFunc: { ip in
    ip.max()
  })
}

// Min

@inline(never)
public func run_IndexPathMinBeginning(_ n: Int) {
  maxMin(n: n, creator: {
   indexPath(size)
  }, maxMinFunc: { ip in
    ip.min()
  })
}

@inline(never)
public func run_IndexPathMinMiddle(_ n: Int) {
  maxMin(n: n, creator: {
    indexPath(size, middle: -1)
  }, maxMinFunc: { ip in
    ip.min()
  })
}

@inline(never)
public func run_IndexPathMinEnd(_ n: Int) {
  maxMin(n: n, creator: {
    indexPath(size, reversed: true)
  }, maxMinFunc: { ip in
    ip.min()
  })
}