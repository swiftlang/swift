//===--- CxxAssignment.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This is a benchmark that tracks how quickly Swift can assign a C++ container
// to another C++ container of the same size.

import TestsUtils
import CxxStdlibPerformance
import CxxStdlib // FIXME(rdar://128520766): this import should be redundant

public let benchmarks = [
  BenchmarkInfo(
    name: "CxxVecU32.assign",
    runFunction: run_CxxVectorOfU32_assign,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: makeVectorOnce),
  BenchmarkInfo(
    name: "CxxSetU32.assign",
    runFunction: run_CxxSetOfU32_assign,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: makeSetOnce),
]

func makeVectorOnce() {
  initVector(vectorSize)
}

func makeSetOnce() {
  initSet(setSize)
}

let vectorSize = 25_000
let setSize = 1_000
let iterRepeatFactor = 100

@inline(never)
public func run_CxxVectorOfU32_assign(_ n: Int) {
  let source = makeVector32(vectorSize)
  var destination = source
  for _ in 0..<(n * iterRepeatFactor) {
    destination = source
    blackHole(destination)
  }
}

@inline(never)
public func run_CxxSetOfU32_assign(_ n: Int) {
  let source = makeSet32(setSize)
  var destination = source
  for _ in 0..<(n * iterRepeatFactor) {
    destination = source
    blackHole(destination)
  }
}
