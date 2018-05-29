//===--- RemoveWhere.swift ------------------------------------------------===//
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

public let RemoveWhere = [
  // tests repeated remove(at:) calls in generic code
  BenchmarkInfo(name: "RemoveWhereQuadraticStrings", runFunction: run_RemoveWhereQuadraticStrings, tags: [.validation, .api], setUpFunction: buildWorkload),
  BenchmarkInfo(name: "RemoveWhereQuadraticInts", runFunction: run_RemoveWhereQuadraticInts, tags: [.validation, .api], setUpFunction: buildWorkload),
  // tests performance of RangeReplaceableCollection.filter
  BenchmarkInfo(name: "RemoveWhereFilterStrings", runFunction: run_RemoveWhereFilterStrings, tags: [.validation, .api], setUpFunction: buildWorkload),
  BenchmarkInfo(name: "RemoveWhereFilterInts", runFunction: run_RemoveWhereFilterInts, tags: [.validation, .api], setUpFunction: buildWorkload),
  // these two variants test the impact of reference counting and
  // swapping/moving
  BenchmarkInfo(name: "RemoveWhereMoveStrings", runFunction: run_RemoveWhereMoveStrings, tags: [.validation, .api], setUpFunction: buildWorkload),
  BenchmarkInfo(name: "RemoveWhereMoveInts", runFunction: run_RemoveWhereMoveInts, tags: [.validation, .api], setUpFunction: buildWorkload),
  BenchmarkInfo(name: "RemoveWhereSwapStrings", runFunction: run_RemoveWhereSwapStrings, tags: [.validation, .api], setUpFunction: buildWorkload),
  BenchmarkInfo(name: "RemoveWhereSwapInts", runFunction: run_RemoveWhereSwapInts, tags: [.validation, .api], setUpFunction: buildWorkload),
  // these test performance of filter, character iteration/comparison 
  BenchmarkInfo(name: "RemoveWhereFilterString", runFunction: run_RemoveWhereFilterString, tags: [.validation, .api], setUpFunction: buildWorkload),
  BenchmarkInfo(name: "RemoveWhereQuadraticString", runFunction: run_RemoveWhereQuadraticString, tags: [.validation, .api], setUpFunction: buildWorkload),
]

extension RangeReplaceableCollection {
  mutating func removeWhere_quadratic(where match: (Element) throws -> Bool) rethrows {
    for i in indices.reversed() {
      if try match(self[i]) {
        remove(at: i)
      }
    }
  }

  mutating func removeWhere_filter(where match: (Element) throws -> Bool) rethrows {
    try self = self.filter { try !match($0) }
  }
}

extension RangeReplaceableCollection where Self: MutableCollection {
  mutating func removeWhere_move(where match: (Element) throws -> Bool) rethrows  {
    guard var i = try firstIndex(where: match) else { return }

    var j = index(after: i)
    while j != endIndex {
      let element = self[j]
      if try !match(element) {
        self[i] = element
        formIndex(after: &i)
      }
      formIndex(after: &j)
    }

    removeSubrange(i...)
  }

  mutating func removeWhere_swap(where match: (Element) throws -> Bool) rethrows {
    guard var i = try firstIndex(where: match) else { return }

    var j = index(after: i)
    while j != endIndex {
      if try !match(self[i]) {
        self.swapAt(i,j)
        formIndex(after: &i)
      }
      formIndex(after: &j)
    }

    removeSubrange(i...)
  }
}

func testQuadratic<C: RangeReplaceableCollection>(workload: inout C) {
  var i = 0
  workload.removeWhere_quadratic { _ in
    i = i &+ 1
    return i%8 == 0
  }
}

func testFilter<C: RangeReplaceableCollection>(workload: inout C) {
  var i = 0
  workload.removeWhere_filter { _ in
    i = i &+ 1
    return i%8 == 0
  }
}

func testMove<C: RangeReplaceableCollection & MutableCollection>(workload: inout C) {
  var i = 0
  workload.removeWhere_move { _ in
    i = i &+ 1
    return i%8 == 0
  }
}

func testSwap<C: RangeReplaceableCollection & MutableCollection>(workload: inout C) {
  var i = 0
  workload.removeWhere_swap { _ in
    i = i &+ 1
    return i%8 == 0
  }
}

let n = 10_000
let strings = (0..<n).map({ "\($0): A long enough string to defeat the SSO" })
let ints = Array(0..<n)
let str = String(repeating: "A very long ASCII string.", count: n/50)

func buildWorkload() {
  _ = strings.count
  _ = ints.count
  _ = str.count
}


@inline(never)
func run_RemoveWhereQuadraticStrings(_ scale: Int) {
  for _ in 0..<scale {
    var workload = strings; workload.swapAt(0,1)
    testQuadratic(workload: &workload)
  }
}

@inline(never)
func run_RemoveWhereQuadraticInts(_ scale: Int) {
  for _ in 0..<scale {
    var workload = ints; workload.swapAt(0,1)
    testQuadratic(workload: &workload)
  }
}

@inline(never)
func run_RemoveWhereFilterStrings(_ scale: Int) {
  for _ in 0..<scale {
    var workload = strings; workload.swapAt(0,1)
    testFilter(workload: &workload)
  }
}

@inline(never)
func run_RemoveWhereFilterInts(_ scale: Int) {
  for _ in 0..<scale {
    var workload = ints; workload.swapAt(0,1)
    testFilter(workload: &workload)
  }
}

@inline(never)
func run_RemoveWhereMoveStrings(_ scale: Int) {
  for _ in 0..<scale {
    var workload = strings; workload.swapAt(0,1)
    testMove(workload: &workload)
  }
}

@inline(never)
func run_RemoveWhereMoveInts(_ scale: Int) {
  for _ in 0..<scale {
    var workload = ints; workload.swapAt(0,1)
    testMove(workload: &workload)
  }
}

@inline(never)
func run_RemoveWhereSwapStrings(_ scale: Int) {
  for _ in 0..<scale {
    var workload = strings; workload.swapAt(0,1)
    testSwap(workload: &workload)
  }
}

@inline(never)
func run_RemoveWhereSwapInts(_ scale: Int) {
  for _ in 0..<scale {
    var workload = ints; workload.swapAt(0,1)
    testSwap(workload: &workload)
  }
}

@inline(never)
func run_RemoveWhereFilterString(_ scale: Int) {
  for _ in 0..<scale {
    var workload = str
    workload.append("!")
    testFilter(workload: &workload)
  }
}

@inline(never)
func run_RemoveWhereQuadraticString(_ scale: Int) {
  for _ in 0..<scale {
    var workload = str
    workload.append("!")
    testQuadratic(workload: &workload)
  }
}

