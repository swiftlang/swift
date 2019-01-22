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
public let ArrayInClass = BenchmarkInfo(
  name: "ArrayInClass",
  runFunction: run_ArrayInClass,
  tags: [.validation, .api, .Array],
  setUpFunction: { ac = ArrayContainer() },
  tearDownFunction: { ac = nil })

var ac: ArrayContainer!

class ArrayContainer {
  final var arr : [Int]

  init() {
    arr = [Int] (repeating: 0, count: 100_000)
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
