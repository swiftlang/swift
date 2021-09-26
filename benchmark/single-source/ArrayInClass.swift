//===--- ArrayInClass.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
public let benchmarks = [
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
    setUpFunction: { workload = ClassWithArrs(n: 10_000) },
    tearDownFunction: { workload = nil }),
]

var ac: ArrayContainer!

class ArrayContainer {
  final var arr : [Int]

  init() {
    arr = [Int] (repeating: 0, count: 20_000)
  }

  func runLoop(_ n: Int) {
    for _ in 0 ..< n {
      for i in 0 ..< arr.count {
        arr[i] = arr[i] + 1
      }
    }
  }
}

@inline(never)
public func run_ArrayInClass(_ n: Int) {
  let a = ac!
  a.runLoop(n)
}

class ClassWithArrs {
  var n: Int = 0
  var a: [Int]
  var b: [Int]

  init(n: Int) {
    self.n = n

    a = [Int](repeating: 0, count: n)
    b = [Int](repeating: 0, count: n)
  }

  func readArr() {
    for i in 0..<self.n {
      guard a[i] == b[i] else { fatalError("") }
    }
  }

  func writeArr() {
    for i in 0..<self.n {
      a[i] = i
      b[i] = i
    }
  }
}

var workload: ClassWithArrs!

public func run_DistinctClassFieldAccesses(_ n: Int) {
  for _ in 1...n {
    workload.writeArr()
    workload.readArr()
  }
}
