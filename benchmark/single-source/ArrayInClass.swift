//===--- ArrayInClass.swift -----------------------------------------------===//
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
public let ArrayInClass = [
  BenchmarkInfo(
    name: "ArrayInClass",
    runFunction: run_ArrayInClass,
    tags: [.validation, .api, .Array],
    setUpFunction: { ac = ArrayContainer() },
    tearDownFunction: { ac = nil },
    legacyFactor: 5),
  BenchmarkInfo(name: "DistinctClassFieldAccesses",
    runFunction: run_DistinctClassFieldAccesses,
    tags: [.validation, .api, .Array],
    setUpFunction: { workload = ClassWithArrs(N: 10_000) },
    tearDownFunction: { workload = nil }),
]

var ac: ArrayContainer!

class ArrayContainer {
  final var arr : [Int]

  init() {
    arr = [Int] (repeating: 0, count: 20_000)
  }

  func runLoop(_ N: Int) {
    for _ in 0 ..< N {
      for i in 0 ..< arr.count {
        arr[i] = arr[i] + 1
      }
    }
  }
}

@inline(never)
public func run_ArrayInClass(_ N: Int) {
  let a = ac!
  a.runLoop(N)
}

class ClassWithArrs {
  var N: Int = 0
  var A: [Int]
  var B: [Int]

  init(N: Int) {
    self.N = N

    A = [Int](repeating: 0, count: N)
    B = [Int](repeating: 0, count: N)
  }

  func readArr() {
    for i in 0..<self.N {
      guard A[i] == B[i] else { fatalError("") }
    }
  }

  func writeArr() {
    for i in 0..<self.N {
      A[i] = i
      B[i] = i
    }
  }
}

var workload: ClassWithArrs!

public func run_DistinctClassFieldAccesses(_ N: Int) {
  for _ in 1...N {
    workload.writeArr()
    workload.readArr()
  }
}
