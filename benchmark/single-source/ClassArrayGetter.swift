//===--- ClassArrayGetter.swift -------------------------------------------===//
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

public let ClassArrayGetter = BenchmarkInfo(
  name: "ClassArrayGetter2",
  runFunction: run_ClassArrayGetter,
  tags: [.validation, .api, .Array],
  setUpFunction: { blackHole(inputArray) },
  tearDownFunction: { inputArray = nil })

class Box {
  var v: Int
  init(v: Int) { self.v = v }
}

@inline(never)
func sumArray(_ a: [Box]) -> Int {
  var s = 0
  for i in 0..<a.count {
    s += a[i].v
  }
  return s
}

var inputArray: [Box]! = {
  let aSize = 100_000
  var a: [Box] = []
  a.reserveCapacity(aSize)
  for i in 1...aSize {
    a.append(Box(v:i))
  }
  return a
}()

public func run_ClassArrayGetter(_ N: Int) {
  let a: [Box] = inputArray
  for _ in 1...N {
    _ = sumArray(a)
  }
}
